
open Lwt
open Ex_common

let http_client ?ca ?fp hostname port =
  Mirage_crypto_rng_lwt.initialize () >>= fun () ->
  let port          = int_of_string port in
  auth ~hostname ?ca ?fp () >>= fun authenticator ->
  Tls_lwt.connect_ext
    (Tls.Config.client ~authenticator ())
    (hostname, port) >>= fun (ic, oc) ->
  let req = String.concat "\r\n" [
    "GET / HTTP/1.1" ; "Host: " ^ hostname ; "Connection: close" ; "" ; ""
  ] in
  Lwt_io.(write oc req >>= fun () -> read ic >>= print >>= fun () -> printf "++ done.\n%!")

let () =
  try
    match Sys.argv with
    | [| _ ; host ; port ; "FP" ; fp |] -> Lwt_main.run (http_client host port ~fp)
    | [| _ ; host ; port ; trust |] -> Lwt_main.run (http_client host port ~ca:trust)
    | [| _ ; host ; port |]         -> Lwt_main.run (http_client host port)
    | [| _ ; host |]                -> Lwt_main.run (http_client host "443")
    | args                          -> Printf.eprintf "%s <host> <port>\n%!" args.(0)
  with
  | Tls_lwt.Tls_alert alert as exn ->
      print_alert "remote end" alert ; raise exn
  | Tls_lwt.Tls_failure fail as exn ->
      print_fail "our end" fail ; raise exn

