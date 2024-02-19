(*
 * Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Lwt.Infix

let () = Ssl.init ()

let chans_of_fd sock =
  let is_open = ref true in
  let shutdown () =
    if !is_open then Lwt_ssl.ssl_shutdown sock else Lwt.return_unit
  in
  let close () =
    is_open := false;
    Lwt_ssl.close sock
  in
  let oc =
    Lwt_io.make ~mode:Lwt_io.output ~close:shutdown (Lwt_ssl.write_bytes sock)
  in
  let ic = Lwt_io.make ~mode:Lwt_io.input ~close (Lwt_ssl.read_bytes sock) in
  (Lwt_ssl.get_fd sock, ic, oc)

module Client = struct
  type context = Ssl.context

  let create_ctx ?certfile ?keyfile ?password () =
    let ctx = Ssl.create_context Ssl.SSLv23 Ssl.Client_context in
    Ssl.disable_protocols ctx [ Ssl.SSLv23 ];
    (* Use default CA certificates *)
    ignore (Ssl.set_default_verify_paths ctx);
    (* Enable peer verification *)
    Ssl.set_verify ctx [ Ssl.Verify_peer ] None;
    (match (certfile, keyfile) with
    | Some certfile, Some keyfile -> Ssl.use_certificate ctx certfile keyfile
    | None, _ | _, None -> ());
    (match password with
    | Some password -> Ssl.set_password_callback ctx password
    | None -> ());
    ctx

  let default_ctx = create_ctx ()

  type verify = { hostname : bool; ip : bool }

  let default_verify = { hostname = true; ip = false }

  let validate_hostname host_addr =
    try
      let _ = Domain_name.(host_exn (of_string_exn host_addr)) in
      host_addr
    with Invalid_argument msg ->
      let s =
        Printf.sprintf "couldn't convert %s to a [`host] Domain_name.t: %s"
          host_addr msg
      in
      invalid_arg s

  let verification { hostname; ip } = function
    | None, _ when hostname -> invalid_arg "impossible to verify hostname"
    | _, None when ip -> invalid_arg "impossible to verify ip"
    | h, i ->
        let hostname =
          if hostname && h <> None then Option.map validate_hostname h else None
        in
        let ip = if ip && i <> None then i else None in
        (hostname, ip)

  let connect ?(ctx = default_ctx) ?src ?hostname ?ip ?verify sa =
    let verify = Option.value ~default:default_verify verify in
    let to_verify = verification verify (hostname, ip) in
    Conduit_lwt_server.with_socket sa (fun fd ->
        (match src with
        | None -> Lwt.return_unit
        | Some src_sa -> Lwt_unix.bind fd src_sa)
        >>= fun () ->
        Lwt_unix.connect fd sa >>= fun () ->
        let with_socket f =
          let s = Lwt_ssl.embed_uninitialized_socket fd ctx in
          let socket = Lwt_ssl.ssl_socket_of_uninitialized_socket s in
          f socket;
          Lwt_ssl.ssl_perform_handshake s
        in
        let maybe_verify ssl = function
          | Some hostname, Some ip ->
              Ssl.set_hostflags ssl [ Ssl.No_partial_wildcards ];
              Ssl.set_client_SNI_hostname ssl hostname;
              Ssl.set_host ssl hostname;
              Ssl.set_ip ssl (Ipaddr.to_string ip)
          | Some hostname, None ->
              Ssl.set_hostflags ssl [ Ssl.No_partial_wildcards ];
              Ssl.set_client_SNI_hostname ssl hostname;
              Ssl.set_host ssl hostname
          | None, Some ip -> Ssl.set_ip ssl (Ipaddr.to_string ip)
          | None, None -> ()
        in
        with_socket (fun ssl -> maybe_verify ssl to_verify) >>= fun sock ->
        Lwt.return (chans_of_fd sock))
end

module Server = struct
  let default_ctx = Ssl.create_context Ssl.SSLv23 Ssl.Server_context
  let () = Ssl.disable_protocols default_ctx [ Ssl.SSLv23 ]

  let listen ?(ctx = default_ctx) ?backlog ?password ~certfile ~keyfile sa =
    let fd = Conduit_lwt_server.listen ?backlog sa in
    (match password with
    | None -> ()
    | Some fn -> Ssl.set_password_callback ctx fn);
    Ssl.use_certificate ctx certfile keyfile;
    fd

  let init ?(ctx = default_ctx) ?backlog ?password ~certfile ~keyfile ?stop
      ?timeout sa cb =
    sa
    |> listen ~ctx ?backlog ?password ~certfile ~keyfile
    >>= Conduit_lwt_server.init ?stop (fun (fd, addr) ->
            Lwt.try_bind
              (fun () -> Lwt_ssl.ssl_accept fd ctx)
              (fun sock -> Lwt.return (chans_of_fd sock))
              (fun exn -> Lwt_unix.close fd >>= fun () -> Lwt.fail exn)
            >>= Conduit_lwt_server.process_accept ?timeout (cb addr))
end

let available = true
