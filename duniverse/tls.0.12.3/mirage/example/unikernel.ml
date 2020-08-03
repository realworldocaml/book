open Lwt.Infix

let escape_data buf = String.escaped (Cstruct.to_string buf)

let make_tracer dump =
  let traces = ref [] in
  let trace sexp =
    traces := Sexplib.Sexp.to_string_hum sexp :: !traces
  and flush () =
    let msgs = List.rev !traces in
    traces := [] ;
    Lwt_list.iter_s dump msgs in
  (trace, flush)

module Server (S  : Mirage_stack.V4)
              (KV : Mirage_kv.RO)
              (CL : Mirage_clock.PCLOCK) =
struct

  module TLS  = Tls_mirage.Make (S.TCPV4)
  module X509 = Tls_mirage.X509 (KV) (CL)

  let rec handle flush tls =
    TLS.read tls >>= fun res ->
    flush () >>= fun () ->
    match res with
    | Ok (`Data buf) ->
      Logs_lwt.info (fun p -> p "recv %s" (escape_data buf)) >>= fun () ->
      (TLS.write tls buf >>= function
        | Ok () -> handle flush tls
        | Error e -> Logs_lwt.err (fun p -> p "write error %a" TLS.pp_write_error e))
    | Ok `Eof -> Logs_lwt.info (fun p -> p "eof from server")
    | Error e -> Logs_lwt.err (fun p -> p "read error %a" TLS.pp_error e)

  let accept conf k flow =
    let trace, flush_trace =
      make_tracer (fun s -> Logs_lwt.debug (fun p -> p "%s" s))
    in
    Logs_lwt.info (fun p -> p "accepted.") >>= fun () ->
    TLS.server_of_flow ~trace conf flow >>= function
    | Ok tls -> Logs_lwt.info (fun p -> p "shook hands") >>= fun () -> k flush_trace tls
    | Error e -> Logs_lwt.err (fun p -> p "%a" TLS.pp_write_error e)

  let start stack kv _ _ =
    X509.certificate kv `Default >>= fun cert ->
    let conf = Tls.Config.server ~certificates:(`Single cert) () in
    S.listen_tcpv4 stack ~port:4433 (accept conf handle) ;
    S.listen stack

end

module Client (S  : Mirage_stack.V4)
              (KV : Mirage_kv.RO)
              (CL : Mirage_clock.PCLOCK) =
struct

  module TLS  = Tls_mirage.Make (S.TCPV4)
  module X509 = Tls_mirage.X509 (KV) (CL)

  open Ipaddr

  let peer = ((V4.of_string_exn "127.0.0.1", 4433), "localhost")
  let peer = ((V4.of_string_exn "2.19.157.15", 443), "www.apple.com")
  let peer = ((V4.of_string_exn "74.125.195.103", 443), "www.google.com")
  let peer = ((V4.of_string_exn "10.0.0.1", 4433), "localhost")
  let peer = ((V4.of_string_exn "23.253.164.126", 443), "tls.openmirage.org")
  let peer = ((V4.of_string_exn "216.105.38.15", 443), "slashdot.org")
  let peer = ((V4.of_string_exn "46.43.42.136", 443), "mirage.io")
  let peer = ((V4.of_string_exn "198.167.222.205", 443), "hannes.nqsb.io")

  let initial = Cstruct.of_string @@
    "GET / HTTP/1.1\r\nConnection: Close\r\nHost: " ^ snd peer ^ "\r\n\r\n"

  let chat tls =
    let rec dump () =
      TLS.read tls >>= function
      | Ok (`Data buf) -> Logs_lwt.info (fun p -> p "recv %s" (escape_data buf)) >>= dump
      | Ok `Eof -> Logs_lwt.info (fun p -> p "eof")
      | Error e -> Logs_lwt.err (fun p -> p "chat err %a" TLS.pp_error e)
    in
    TLS.write tls initial >>= function
    | Ok () -> dump ()
    | Error e -> Logs_lwt.err (fun p -> p "write error %a" TLS.pp_write_error e)

  let start stack kv _clock _ =
    X509.authenticator kv `CAs >>= fun authenticator ->
    let conf = Tls.Config.client ~authenticator () in
    S.TCPV4.create_connection (S.tcpv4 stack) (fst peer)
    >>= function
    | Error e -> Logs_lwt.err (fun p -> p "%a" S.TCPV4.pp_error e)
    | Ok tcp  ->
      TLS.client_of_flow conf ~host:(snd peer) tcp >>= function
      | Ok tls -> chat tls
      | Error e -> Logs_lwt.err (fun p -> p "%a" TLS.pp_write_error e)

end
