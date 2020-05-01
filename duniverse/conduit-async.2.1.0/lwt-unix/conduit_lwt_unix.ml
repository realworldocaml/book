(*
 * Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014 Hannes Mehnert <hannes@mehnert.org>
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
open Sexplib.Conv

let debug = ref false
let debug_print = ref Printf.eprintf
let () =
  try
    ignore(Sys.getenv "CONDUIT_DEBUG");
    debug := true
  with Not_found -> ()

type tls_lib = | OpenSSL | Native | No_tls [@@deriving sexp]
let default_tls_library =
  (* TODO build time selection *)
  let default =
    if Conduit_lwt_tls.available then
      Native
    else if Conduit_lwt_unix_ssl.available then
      OpenSSL
    else
      No_tls
  in
  match String.lowercase_ascii (Sys.getenv "CONDUIT_TLS") with
  | "native" -> Native
  | "openssl" | "libressl" -> OpenSSL
  | "none" | "notls" -> No_tls
  | _ -> default
  | exception  Not_found -> default

let tls_library = ref default_tls_library

let () = if !debug then
  !debug_print "Selected TLS library: %s\n"
    (Sexplib.Sexp.to_string (sexp_of_tls_lib !tls_library))

type +'a io = 'a Lwt.t
type ic = Lwt_io.input_channel
type oc = Lwt_io.output_channel

type client_tls_config =
  [ `Hostname of string ] *
  [ `IP of Ipaddr_sexp.t ] *
  [ `Port of int ]
[@@deriving sexp]

type client = [
  | `TLS of client_tls_config
  | `TLS_native of client_tls_config
  | `OpenSSL of client_tls_config
  | `TCP of [ `IP of Ipaddr_sexp.t ] * [`Port of int ]
  | `Unix_domain_socket of [ `File of string ]
  | `Vchan_direct of [ `Domid of int ] * [ `Port of string ]
  | `Vchan_domain_socket of [ `Domain_name of string ] * [ `Port of string ]
] [@@deriving sexp]

(** Configuration fragment for a listening TLS server *)
type server_tls_config =
  [ `Crt_file_path of string ] *
  [ `Key_file_path of string ] *
  [ `Password of bool -> string | `No_password ] *
  [ `Port of int ]
[@@deriving sexp]

(** Set of ways to create TCP servers *)
type tcp_config = [
  | `Port of int
  | `Socket of (Lwt_unix.file_descr [@sexp.opaque])
] [@@deriving sexp]

(** Set of supported listening mechanisms that are supported by this module. *)
type server = [
  | `TLS of server_tls_config
  | `OpenSSL of server_tls_config
  | `TLS_native of server_tls_config
  | `TCP of tcp_config
  | `Unix_domain_socket of [ `File of string ]
  | `Vchan_direct of int * string
  | `Vchan_domain_socket of string  * string
  | `Launchd of string
] [@@deriving sexp]

type tls_own_key = [
  | `None
  | `TLS of
      [ `Crt_file_path of string ] *
      [ `Key_file_path of string ] *
      [ `Password of bool -> string | `No_password ]
] [@@deriving sexp]

type tls_server_key = tls_own_key [@@deriving sexp]

type ctx = {
  src: Unix.sockaddr option;
  tls_own_key: tls_own_key;
}

let string_of_unix_sockaddr sa =
  let open Unix in
  match sa with
  | ADDR_UNIX s ->
      Printf.sprintf "ADDR_UNIX(%s)" s
  | ADDR_INET (ia, port) ->
      Printf.sprintf "ADDR_INET(%s,%d)" (string_of_inet_addr ia) port

let sexp_of_ctx ctx =
  [%sexp_of: string option * tls_own_key ]
    ((match ctx.src with
      | None -> None
      | Some sa -> Some (string_of_unix_sockaddr sa)),
     ctx.tls_own_key)

type tcp_flow = {
  fd: (Lwt_unix.file_descr [@sexp.opaque]);
  ip: Ipaddr_sexp.t;
  port: int;
} [@@deriving sexp]

type domain_flow = {
  fd: (Lwt_unix.file_descr [@sexp.opaque]);
  path: string;
} [@@deriving sexp]

type vchan_flow = {
  domid: int;
  port: string;
} [@@deriving sexp]

type flow =
  | TCP of tcp_flow
  | Domain_socket of domain_flow
  | Vchan of vchan_flow
[@@deriving sexp]

let flow_of_fd fd sa =
  match sa with
  | Unix.ADDR_UNIX path -> Domain_socket { fd; path }
  | Unix.ADDR_INET (ip,port) -> TCP { fd; ip=Ipaddr_unix.of_inet_addr ip; port }

let default_ctx =
  { src=None; tls_own_key=`None }

let init ?src ?(tls_own_key=`None) ?(tls_server_key=`None) () =
  let tls_own_key =
    match tls_own_key with `None -> tls_server_key | _ -> tls_own_key in
  match src with
  | None ->
    Lwt.return { src=None; tls_own_key }
  | Some host ->
    let open Unix in
    Lwt_unix.getaddrinfo host "0" [AI_PASSIVE; AI_SOCKTYPE SOCK_STREAM]
    >>= function
    | {ai_addr;_}::_ -> Lwt.return { src=Some ai_addr; tls_own_key }
    | [] -> Lwt.fail_with "Invalid conduit source address specified"

module Sockaddr_io = struct
  let shutdown_no_exn fd mode =
    try Lwt_unix.shutdown fd mode
    with Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()

  let make_fd_state () =
    ref `Open

  let make fd =
    let fd_state = make_fd_state () in
    let close_in () =
      match !fd_state with
      | `Open -> fd_state := `In_closed; shutdown_no_exn fd Unix.SHUTDOWN_RECEIVE; Lwt.return_unit
      | `Out_closed -> fd_state := `Closed; Lwt_unix.close fd
      | `In_closed (* repeating on a closed channel is a noop in Lwt_io *)
      | `Closed -> Lwt.return_unit  in
    let close_out () =
      match !fd_state with
      | `Open -> fd_state := `Out_closed; shutdown_no_exn fd Unix.SHUTDOWN_SEND; Lwt.return_unit
      | `In_closed -> fd_state := `Closed; Lwt_unix.close fd
      | `Out_closed (* repeating on a closed channel is a noop in Lwt_io *)
      | `Closed -> Lwt.return_unit  in
    let ic = Lwt_io.of_fd ~close:close_in  ~mode:Lwt_io.input fd in
    let oc = Lwt_io.of_fd ~close:close_out ~mode:Lwt_io.output fd in
    (ic, oc)
end

(* Vanilla sockaddr connection *)
module Sockaddr_client = struct
  let connect ?src sa =
    Conduit_lwt_server.with_socket sa (fun fd ->
        (match src with
         | None -> Lwt.return_unit
         | Some src_sa -> Lwt_unix.bind fd src_sa) >>= fun () ->
        Lwt_unix.connect fd sa >>= fun () ->
        let ic, oc = Sockaddr_io.make fd in
        Lwt.return (fd, ic, oc)
      )
end

module Sockaddr_server = struct

  let set_sockopts_no_exn fd =
    try Lwt_unix.setsockopt fd Lwt_unix.TCP_NODELAY true
    with (* This is expected for Unix domain sockets *)
    | Unix.Unix_error(Unix.EOPNOTSUPP, _, _) -> ()

  let process_accept ?timeout callback (client,peeraddr) =
    set_sockopts_no_exn client;
    let ic, oc = Sockaddr_io.make client in
    let c = callback (flow_of_fd client peeraddr) ic oc in
    let events = match timeout with
      |None -> [c]
      |Some t -> [c; (Lwt_unix.sleep (float_of_int t)) ] in
    Lwt.finalize
      (fun () -> Lwt.pick events)
      (fun () -> Conduit_lwt_server.close (ic,oc))

  let init ~on ?stop ?backlog ?timeout callback =
    (match on with
     | `Socket s -> Lwt.return s
     | `Sockaddr sockaddr -> Conduit_lwt_server.listen ?backlog sockaddr)
    >>= Conduit_lwt_server.init ?stop (process_accept ?timeout callback)
end

let set_max_active maxactive =
  Conduit_lwt_server.set_max_active maxactive

(** TLS client connection functions *)

let connect_with_tls_native ~ctx (`Hostname hostname, `IP ip, `Port port) =
  let sa = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip,port) in
  (match ctx.tls_own_key with
   | `None -> Lwt.return_none
   | `TLS (_, _, `Password _) ->
      Lwt.fail_with "OCaml-TLS cannot handle encrypted pem files"
   | `TLS (`Crt_file_path cert, `Key_file_path priv_key, `No_password) ->
      Conduit_lwt_tls.X509.private_of_pems ~cert ~priv_key >|= fun certificate ->
      Some (`Single certificate)
  ) >>= fun certificates ->
  Conduit_lwt_tls.Client.connect ?src:ctx.src ?certificates hostname sa
  >|= fun (fd, ic, oc) ->
  let flow = TCP { fd ; ip ; port } in
  (flow, ic, oc)

let connect_with_openssl ~ctx (`Hostname hostname, `IP ip, `Port port) =
  let sa = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip,port) in
  let ctx_ssl =
    match ctx.tls_own_key with
    | `None -> None
    | `TLS (`Crt_file_path certfile, `Key_file_path keyfile, password) ->
        let password =
          (match password with
           | `No_password -> None
           | `Password fn -> Some fn) in
        let ctx_ssl =
          Conduit_lwt_unix_ssl.Client.create_ctx ~certfile ~keyfile ?password ()
        in
        Some ctx_ssl
  in
  Conduit_lwt_unix_ssl.Client.connect ?ctx:ctx_ssl ?src:ctx.src ~hostname sa
  >>= fun (fd, ic, oc) ->
  let flow = TCP {fd;ip;port} in
  Lwt.return (flow, ic, oc)

let connect_with_default_tls ~ctx tls_client_config =
  match !tls_library with
  | OpenSSL -> connect_with_openssl ~ctx tls_client_config
  | Native -> connect_with_tls_native ~ctx tls_client_config
  | No_tls -> Lwt.fail_with "No SSL or TLS support compiled into Conduit"

(** Main connection function *)

let connect ~ctx (mode:client) =
  match mode with
  | `TCP (`IP ip, `Port port) ->
    let sa = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip, port) in
    Sockaddr_client.connect ?src:ctx.src sa
    >>= fun (fd, ic, oc) ->
    let flow = TCP {fd;ip;port} in
    Lwt.return (flow, ic, oc)
  | `Unix_domain_socket (`File path) ->
    Sockaddr_client.connect (Unix.ADDR_UNIX path)
    >>= fun (fd, ic, oc) ->
    let flow = Domain_socket {fd; path} in
    Lwt.return (flow, ic, oc)
  | `TLS c -> connect_with_default_tls ~ctx c
  | `OpenSSL c -> connect_with_openssl ~ctx c
  | `TLS_native c -> connect_with_tls_native ~ctx c
  | `Vchan_direct _ -> Lwt.fail_with "Vchan_direct not available on unix"
  | `Vchan_domain_socket _uuid ->
    Lwt.fail_with "Vchan_domain_socket not implemented"

let sockaddr_on_tcp_port ctx port =
  let open Unix in
  match ctx.src with
  | Some (ADDR_UNIX _) -> failwith "Cant listen to TCP on a domain socket"
  | Some (ADDR_INET (a,_)) -> ADDR_INET (a,port), Ipaddr_unix.of_inet_addr a
  | None -> ADDR_INET (inet_addr_any,port), Ipaddr.(V4 V4.any)

let serve_with_openssl ?timeout ?stop ~ctx ~certfile ~keyfile
                       ~pass ~port callback =
  let sockaddr, _ = sockaddr_on_tcp_port ctx port in
  let password =
    match pass with
    | `No_password -> None
    | `Password fn -> Some fn
  in
  Conduit_lwt_unix_ssl.Server.init
    ?password ~certfile ~keyfile ?timeout ?stop sockaddr
    (fun addr fd ic oc -> callback (flow_of_fd fd addr) ic oc)

let serve_with_tls_native ?timeout ?stop ~ctx ~certfile ~keyfile
                          ~pass ~port callback =
  let sockaddr, _ = sockaddr_on_tcp_port ctx port in
  (match pass with
    | `No_password -> Lwt.return ()
    | `Password _ -> Lwt.fail_with "OCaml-TLS cannot handle encrypted pem files"
  ) >>= fun () ->
  Conduit_lwt_tls.Server.init
    ~certfile ~keyfile ?timeout ?stop sockaddr
    (fun addr fd ic oc -> callback (flow_of_fd fd addr) ic oc)

let serve_with_default_tls ?timeout ?stop ~ctx ~certfile ~keyfile
    ~pass ~port callback =
  match !tls_library with
  | OpenSSL -> serve_with_openssl ?timeout ?stop ~ctx ~certfile ~keyfile
                 ~pass ~port callback
  | Native -> serve_with_tls_native ?timeout ?stop ~ctx ~certfile ~keyfile
                ~pass ~port callback
  | No_tls -> failwith "No SSL or TLS support compiled into Conduit"

let serve ?backlog ?timeout ?stop
    ~on_exn
    ~(ctx:ctx) ~(mode:server) callback =
  let callback flow ic oc =
    Lwt.catch
      (fun () -> callback flow ic oc)
      (fun exn -> on_exn exn; Lwt.return_unit)
  in
  match mode with
  | `TCP (`Port port) ->
    let sockaddr, _ = sockaddr_on_tcp_port ctx port in
    Sockaddr_server.init ~on:(`Sockaddr sockaddr) ?backlog ?timeout ?stop callback
  | `TCP (`Socket s) ->
    Sockaddr_server.init ~on:(`Socket s) ?backlog ?timeout ?stop callback
  | `Unix_domain_socket (`File path) ->
    let sockaddr = Unix.ADDR_UNIX path in
    Sockaddr_server.init ~on:(`Sockaddr sockaddr) ?backlog ?timeout ?stop callback
  | `TLS (`Crt_file_path certfile, `Key_file_path keyfile, pass, `Port port) ->
    serve_with_default_tls ?timeout ?stop ~ctx ~certfile ~keyfile
      ~pass ~port callback
  | `OpenSSL (`Crt_file_path certfile, `Key_file_path keyfile,
              pass, `Port port) ->
    serve_with_openssl ?timeout ?stop ~ctx ~certfile ~keyfile
      ~pass ~port callback
  | `TLS_native (`Crt_file_path certfile, `Key_file_path keyfile,
                 pass, `Port port) ->
    serve_with_tls_native ?timeout ?stop ~ctx ~certfile ~keyfile
      ~pass ~port callback
  |`Vchan_direct _ -> Lwt.fail_with "Vchan_direct not implemented"
  | `Vchan_domain_socket _uuid ->
    Lwt.fail_with "Vchan_domain_socket not implemented"
  | `Launchd name ->
    let fn s = Sockaddr_server.init ~on:(`Socket s) ?timeout ?stop callback in
    Conduit_lwt_launchd.activate fn name

let endp_of_flow = function
  | TCP { ip; port; _ } -> `TCP (ip, port)
  | Domain_socket { path; _ } -> `Unix_domain_socket path
  | Vchan { domid; port } -> `Vchan_direct (domid, port)

(** Use the configuration of the server to interpret how to
    handle a particular endpoint from the resolver into a
    concrete implementation of type [client] *)
let endp_to_client ~ctx:_ (endp:Conduit.endp) : client Lwt.t =
  match endp with
  | `TCP (ip, port) -> Lwt.return (`TCP (`IP ip, `Port port))
  | `Unix_domain_socket file -> Lwt.return (`Unix_domain_socket (`File file))
  | `Vchan_direct (domid, port) ->
    Lwt.return (`Vchan_direct (`Domid domid, `Port port))
  | `Vchan_domain_socket (name, port) ->
    Lwt.return (`Vchan_domain_socket (`Domain_name name, `Port port))
  | `TLS (host, (`TCP (ip, port))) ->
    Lwt.return (`TLS (`Hostname host, `IP ip, `Port port))
  | `TLS (host, endp) -> begin
      Lwt.fail_with (Printf.sprintf
                       "TLS to non-TCP currently unsupported: host=%s endp=%s"
                       host (Sexplib.Sexp.to_string_hum (Conduit.sexp_of_endp endp)))
    end
  | `Unknown err -> Lwt.fail_with ("resolution failed: " ^ err)

let endp_to_server ~ctx (endp:Conduit.endp) =
  match endp with
  | `Unix_domain_socket path -> Lwt.return (`Unix_domain_socket (`File path))
  | `TLS (_host, `TCP (_ip, port)) ->
    begin match ctx.tls_own_key with
      | `None -> Lwt.fail_with "No TLS server key configured"
      | `TLS (`Crt_file_path crt, `Key_file_path key, pass) ->
        Lwt.return (`TLS (`Crt_file_path crt, `Key_file_path key,
                          pass, `Port port))
    end
  | `TCP (_ip, port) -> Lwt.return (`TCP (`Port port))
  | `Vchan_direct _ as mode -> Lwt.return mode
  | `Vchan_domain_socket _ as mode -> Lwt.return mode
  | `TLS (_host, _) -> Lwt.fail_with "TLS to non-TCP currently unsupported"
  | `Unknown err -> Lwt.fail_with ("resolution failed: " ^ err)
