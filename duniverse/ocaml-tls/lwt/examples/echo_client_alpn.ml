
open Ex_common
open Lwt

let echo_client host port =
  let open Lwt_io in
  Mirage_crypto_rng_lwt.initialize () >>= fun () ->
  let port = int_of_string port in
  let authenticator = null_auth in
  Tls_lwt.Unix.connect
    Tls.Config.(client ~authenticator ~alpn_protocols:["http/1.1"; "h2"] ())
    (host, port) >>= fun t ->
  match Tls_lwt.Unix.epoch t with
  | `Error -> printl "Error"
  | `Ok epoch -> (
    match epoch.Tls.Core.alpn_protocol with
      | None -> printl "No protocol selected"
      | Some protocol -> printl ("Selected protocol: " ^ protocol)
  )
    >>= fun () -> Tls_lwt.Unix.close t

let () =
  Lwt_main.run (echo_client "127.0.0.1" "4433")
