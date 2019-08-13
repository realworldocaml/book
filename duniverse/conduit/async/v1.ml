open Core
open Async
open Private_ssl.V1

type session = Ssl.session [@@deriving sexp]
type ssl_version = Ssl.version [@@deriving sexp]
type ssl_conn = Ssl.connection [@@deriving sexp]

module Conduit_async = struct
  module Ssl = struct
    include Ssl

    type nonrec config     = Config.t [@@deriving sexp]
    let configure          = Config.create
    let verify_certificate = Config.verify_certificate
  end

  type oc = Writer.t
  type ic = Reader.t
  type 'a io = 'a Deferred.t

  type addr = [
    | `OpenSSL of string * Ipaddr_sexp.t * int
    | `OpenSSL_with_config of string * Ipaddr_sexp.t * int * Ssl.config
    | `TCP of Ipaddr_sexp.t * int
    | `Unix_domain_socket of string
  ] [@@deriving sexp]

  let connect ?interrupt dst =
    match dst with
    | `TCP (ip, port) ->
      let endp = Host_and_port.create ~host:(Ipaddr.to_string ip) ~port in
      Tcp.connect ?interrupt (Tcp.Where_to_connect.of_host_and_port endp)
      >>= fun (_, rd, wr) -> return (rd,wr)
    | `OpenSSL (_, ip, port) ->
      let endp = Host_and_port.create ~host:(Ipaddr.to_string ip) ~port in
      Tcp.connect ?interrupt (Tcp.Where_to_connect.of_host_and_port endp)
      >>= fun (_, rd, wr) ->
      let config = Ssl.configure () in
      Ssl.connect config rd wr
    | `OpenSSL_with_config (_, ip, port, config) ->
      let endp = Host_and_port.create ~host:(Ipaddr.to_string ip) ~port in
      Tcp.connect ?interrupt (Tcp.Where_to_connect.of_host_and_port endp)
      >>= fun (_, rd, wr) ->
      Ssl.connect config rd wr
    | `Unix_domain_socket file ->
      Tcp.connect ?interrupt (Tcp.Where_to_connect.of_file file)
      >>= fun (_, rd, wr) ->
      return (rd,wr)

  let with_connection ?interrupt dst f =
    match dst with
    | `TCP (ip, port) ->
      let endp = Host_and_port.create ~host:(Ipaddr.to_string ip) ~port in
      Tcp.with_connection ?interrupt
        (Tcp.Where_to_connect.of_host_and_port endp)
        (fun _ rd wr -> f rd wr)
    | `OpenSSL (_, ip, port) ->
      let config = Ssl.configure () in
      let endp = Host_and_port.create ~host:(Ipaddr.to_string ip) ~port in
      Tcp.with_connection ?interrupt
        (Tcp.Where_to_connect.of_host_and_port endp)
        begin fun _ rd wr ->
          Ssl.connect config rd wr >>= fun (rd, wr) ->
          Monitor.protect (fun () -> f rd wr) ~finally:begin fun () ->
            Deferred.all_unit [ Reader.close rd ; Writer.close wr ]
          end
        end
    | `OpenSSL_with_config (_, ip, port, config) ->
      let endp = Host_and_port.create ~host:(Ipaddr.to_string ip) ~port in
      Tcp.with_connection ?interrupt
        (Tcp.Where_to_connect.of_host_and_port endp)
        begin fun _ rd wr ->
          Ssl.connect config rd wr >>= fun (rd, wr) ->
          Monitor.protect (fun () -> f rd wr) ~finally:begin fun () ->
            Deferred.all_unit [ Reader.close rd ; Writer.close wr ]
          end
        end
    | `Unix_domain_socket file ->
      Tcp.with_connection ?interrupt (Tcp.Where_to_connect.of_file file)
        (fun _ rd wr -> f rd wr)

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
        Ssl.listen
          ?ca_file ?ca_path ~crt_file ~key_file rd wr >>= fun (rd,wr) ->
        Monitor.protect
          (fun () -> handle_request sock rd wr)
          ~finally:(fun () ->
              Deferred.all_unit [ Reader.close rd ; Writer.close wr ])
    in
    Tcp.Server.create ?max_connections ?backlog
      ?buffer_age_limit ~on_handler_error
      where_to_listen (handle_client handle_request)
end

module Conduit_async_ssl = struct
  module Ssl_config = Conduit_async.Ssl
  let ssl_connect = Ssl.connect
  let ssl_listen = Ssl.listen
end
