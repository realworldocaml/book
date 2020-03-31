
open Lwt
open Ex_common

let test_client _ =
  let port = 4433 in
  let host = "127.0.0.1" in
  X509_lwt.authenticator `No_authentication_I'M_STUPID >>= fun authenticator ->
  Tls_lwt.connect_ext
    Tls.Config.(client ~authenticator ~ciphers:Ciphers.supported ())
    (host, port) >>= fun (ic, oc) ->
  let req = String.concat "\r\n" [
    "GET / HTTP/1.1" ; "Host: " ^ host ; "Connection: close" ; "" ; ""
  ] in
  Lwt_io.(write oc req >>= fun () -> read ic >>= print >>= fun () -> printf "++ done.\n%!")

let () =
  try
    Lwt_main.run (test_client ())
  with
  | Tls_lwt.Tls_alert alert as exn ->
      print_alert "remote end" alert ; raise exn
  | Tls_lwt.Tls_failure alert as exn ->
      print_fail "our end" alert ; raise exn

