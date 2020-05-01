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
  let shutdown () = if !is_open then Lwt_ssl.ssl_shutdown sock else Lwt.return_unit in
  let close () = is_open := false; Lwt_ssl.close sock in
  let oc = Lwt_io.make ~mode:Lwt_io.output ~close:shutdown (Lwt_ssl.write_bytes sock) in
  let ic = Lwt_io.make ~mode:Lwt_io.input ~close (Lwt_ssl.read_bytes sock) in
  ((Lwt_ssl.get_fd sock), ic, oc)

module Client = struct
  let create_ctx ?certfile ?keyfile ?password () =
    let ctx = Ssl.create_context Ssl.SSLv23 Ssl.Client_context in
    Ssl.disable_protocols ctx [Ssl.SSLv23];
    (* Use default CA certificates *)
    ignore (Ssl.set_default_verify_paths ctx);
    (* Enable peer verification *)
    Ssl.set_verify ctx [Ssl.Verify_peer] None;
    (match certfile, keyfile with
     | Some certfile, Some keyfile -> Ssl.use_certificate ctx certfile keyfile
     | None, _ | _, None -> ());
    (match password with
     | Some password -> Ssl.set_password_callback ctx password
     | None -> ());
    ctx

  let default_ctx = create_ctx ()

  let connect ?(ctx=default_ctx) ?src ?hostname sa =
    Conduit_lwt_server.with_socket sa (fun fd ->
        (match src with
         | None        -> Lwt.return_unit
         | Some src_sa -> Lwt_unix.bind fd src_sa
        ) >>= fun () ->
        Lwt_unix.connect fd sa >>= fun () ->
        begin match hostname with
          | Some host ->
            let s = Lwt_ssl.embed_uninitialized_socket fd ctx in
            let ssl = Lwt_ssl.ssl_socket_of_uninitialized_socket s in
            Ssl.set_client_SNI_hostname ssl host;
            (* Enable hostname verification *)
            Ssl.set_hostflags ssl [Ssl.No_partial_wildcards];
            Ssl.set_host ssl host;
            Lwt_ssl.ssl_perform_handshake s
          | None ->
            Lwt_ssl.ssl_connect fd ctx
        end >>= fun sock ->
        Lwt.return (chans_of_fd sock)
      )
end

module Server = struct

  let default_ctx = Ssl.create_context Ssl.SSLv23 Ssl.Server_context
  let () = Ssl.disable_protocols default_ctx [Ssl.SSLv23]

  let listen ?(ctx=default_ctx) ?backlog ?password ~certfile ~keyfile sa =
    let fd = Conduit_lwt_server.listen ?backlog sa in
    (match password with
     | None -> ()
     | Some fn -> Ssl.set_password_callback ctx fn);
    Ssl.use_certificate ctx certfile keyfile;
    fd

  let init ?(ctx=default_ctx) ?backlog ?password ~certfile ~keyfile ?stop
      ?timeout sa cb =
    sa
    |> listen ~ctx ?backlog ?password ~certfile ~keyfile
    >>= Conduit_lwt_server.init ?stop (fun (fd, addr) ->
        Lwt.try_bind (fun () -> Lwt_ssl.ssl_accept fd ctx)
          (fun sock -> Lwt.return (chans_of_fd sock))
          (fun exn -> Lwt_unix.close fd >>= fun () -> Lwt.fail exn)
        >>= Conduit_lwt_server.process_accept ?timeout (cb addr))

end

let available = true
