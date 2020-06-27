
open Lwt
open Ex_common

let serve_ssl port callback =

  let tag = "server" in

  X509_lwt.private_of_pems
    ~cert:server_cert
    ~priv_key:server_key >>= fun certificate ->
  let config =
    Tls.Config.(server ~version:(`TLS_1_0, `TLS_1_3) ~certificates:(`Single certificate) ~ciphers:Ciphers.supported ()) in

  let server_s =
    let open Lwt_unix in
    let s = socket PF_INET SOCK_STREAM 0 in
    setsockopt s Unix.SO_REUSEADDR true ;
    bind s (ADDR_INET (Unix.inet_addr_any, port)) >|= fun () ->
    listen s 10 ;
    s in

  yap ~tag ("-> start @ " ^ string_of_int port) >>= fun () ->
  server_s >>= fun s ->
  Tls_lwt.accept_ext config s >>= fun (channels, addr) ->
  yap ~tag "-> connect" >>= fun () ->
  callback channels addr >>= fun () ->
  yap ~tag "<- handler done"

let test_server port =
  Mirage_crypto_rng_lwt.initialize () >>= fun () ->
  serve_ssl port @@ fun (ic, oc) _addr ->
    yap ~tag:"handler" "accepted" >>= fun () ->
    Lwt_io.read_line ic >>= fun line ->
    yap ~tag:"handler" ("+ " ^ line) >>= fun () ->
    Lwt_io.write_line oc line

let () =
  let port =
    try int_of_string Sys.argv.(1) with _ -> 4433
  in
  Lwt_main.run (test_server port)
