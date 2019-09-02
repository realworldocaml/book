open Core
open Async
open Private_ssl.V2

type _ addr =
  | OpenSSL : Socket.Address.Inet.t * Ssl.Config.t -> Socket.Address.Inet.t addr
  | Inet : Socket.Address.Inet.t -> Socket.Address.Inet.t addr
  | Unix : Socket.Address.Unix.t -> Socket.Address.Unix.t addr
[@@deriving sexp_of]

type _ tcp_sock =
  | Inet_sock :
      ([`Active], Socket.Address.Inet.t) Socket.t ->
      Socket.Address.Inet.t tcp_sock
  | Unix_sock :
      ([`Active], Socket.Address.Unix.t) Socket.t ->
      Socket.Address.Unix.t tcp_sock

let ssl_schemes = [
  "https" ;
  "wss"
]

let mem_scheme s =
  List.mem ssl_schemes ~equal:String.equal s

let resolve_uri ?(options=[]) uri =
  let host =
    Option.value_exn
      ~here:[%here]
      ~message:"no host in URL" (Uri.host uri) in
  let service =
    match Uri.port uri, Uri_services.tcp_port_of_uri uri with
    | Some p, _ -> Some (string_of_int p)
    | None, Some p -> Some (string_of_int p)
    | _ -> None in
  (* Async_extra does not yet support IPv6 *)
  let options = (Unix.Addr_info.AI_FAMILY PF_INET) :: options in
  Unix.Addr_info.get ~host ?service options >>= function
  | [] ->
    failwithf "unable to resolve %s" (Uri.to_string uri) ()
  | { ai_addr; _ } :: _ ->
    match Uri.scheme uri, ai_addr with
    | _, ADDR_UNIX _ ->
      invalid_arg "uri must resolve to inet address"
    | Some s, (ADDR_INET (h, p)) when mem_scheme s ->
      return (OpenSSL ((`Inet (h, p)), Ssl.Config.create ()))
    | _, ADDR_INET (h, p) ->
      return (Inet (`Inet (h, p)))

let connect (type a) ?interrupt (addr: a addr) :
  (a tcp_sock * Reader.t * Writer.t) Deferred.t =
  match addr with
  | Inet addr ->
    Tcp.connect ?interrupt (Tcp.Where_to_connect.of_inet_address addr)
    >>| fun (s, r, w) -> (Inet_sock s, r, w)
  | OpenSSL (addr, cfg) ->
    Tcp.connect ?interrupt (Tcp.Where_to_connect.of_inet_address addr)
    >>= fun (s, rd, wr) -> Ssl.connect ~cfg rd wr >>| fun (rd, wr) ->
    (Inet_sock s, rd, wr)
  | Unix addr ->
    Tcp.connect ?interrupt (Tcp.Where_to_connect.of_unix_address addr)
    >>| fun (s, r, w) -> (Unix_sock s, r, w)

let with_connection (type a) ?interrupt (addr: a addr)
    (f : a tcp_sock -> Reader.t -> Writer.t -> 'a Deferred.t) =
  match addr with
  | Inet addr ->
    Tcp.with_connection ?interrupt
      (Tcp.Where_to_connect.of_inet_address addr)
      (fun s rd wr -> f (Inet_sock s) rd wr)
  | OpenSSL (addr, cfg) ->
    Tcp.with_connection ?interrupt
      (Tcp.Where_to_connect.of_inet_address addr)
      begin fun s rd wr ->
        Ssl.connect ~cfg rd wr >>= fun (rd, wr) ->
        Monitor.protect (fun () -> f (Inet_sock s) rd wr) ~finally:begin fun () ->
          Deferred.all_unit [ Reader.close rd ; Writer.close wr ]
        end
      end
  | Unix addr ->
    Tcp.with_connection ?interrupt (Tcp.Where_to_connect.of_unix_address addr)
      (fun s rd wr -> f (Unix_sock s) rd wr)

let connect_uri ?options ?interrupt uri =
  resolve_uri ?options uri >>= fun addr ->
  connect ?interrupt addr

let with_connection_uri ?options ?interrupt uri f =
  resolve_uri ?options uri >>= fun addr ->
  with_connection ?interrupt addr f

type trust_chain = [
  | `Ca_file of string
  | `Ca_path of string
  | `Search_file_first_then_path of
      [ `File of string ] *
      [ `Path of string ]
] [@@deriving sexp]

type openssl = [
  | `OpenSSL of
      [ `Crt_file_path of string ] *
      [ `Key_file_path of string ]
] [@@deriving sexp]

type requires_async_ssl = [
  | openssl
  | `OpenSSL_with_trust_chain of openssl * trust_chain
] [@@deriving sexp]

type server = [
  | `TCP
  | requires_async_ssl
] [@@deriving sexp]

let serve
    ?max_connections ?backlog
    ?buffer_age_limit ~on_handler_error mode where_to_listen handle_request =
  let handle_client handle_request sock rd wr =
    match mode with
    | `TCP -> handle_request sock rd wr
    | #requires_async_ssl as async_ssl ->
      let (crt_file, key_file, ca_file, ca_path) =
        match async_ssl with
        | `OpenSSL (`Crt_file_path crt_file, `Key_file_path key_file) ->
          (crt_file, key_file, None, None)
        | `OpenSSL_with_trust_chain
            (`OpenSSL (`Crt_file_path crt, `Key_file_path key), trust_chain) ->
          let (ca_file, ca_path) =
            match trust_chain with
            | `Ca_file ca_file -> (Some ca_file, None)
            | `Ca_path ca_path -> (None, Some ca_path)
            | `Search_file_first_then_path (`File ca_file, `Path ca_path) ->
              (Some ca_file, Some ca_path)
          in
          (crt, key, ca_file, ca_path)
      in
      let cfg = Ssl.Config.create
          ?ca_file ?ca_path ~crt_file ~key_file () in
      Ssl.listen cfg rd wr >>= fun (rd,wr) ->
      Monitor.protect
        (fun () -> handle_request sock rd wr)
        ~finally:(fun () ->
            Deferred.all_unit [ Reader.close rd ; Writer.close wr ])
  in
  Tcp.Server.create ?max_connections ?backlog
    ?buffer_age_limit ~on_handler_error
    where_to_listen (handle_client handle_request)

type ssl_version = Ssl.version [@@deriving sexp]
type ssl_opt = Ssl.opt [@@deriving sexp]
type ssl_conn = Ssl.connection [@@deriving sexp_of]
type allowed_ciphers =
  [ `Only of string list | `Openssl_default | `Secure ]
[@@deriving sexp]
type verify_mode = Ssl.verify_mode [@@deriving sexp_of]
type session = Ssl.session [@@deriving sexp_of]
module Ssl = struct
  module Config = Ssl.Config
end
