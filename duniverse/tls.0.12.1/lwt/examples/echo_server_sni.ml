
open Lwt
open Ex_common

let serve_ssl port callback =

  let tag = "server" in

  X509_lwt.private_of_pems
    ~cert:(ca_cert_dir ^ "/bar.pem")
    ~priv_key:server_key >>= fun barcert ->

  X509_lwt.private_of_pems
    ~cert:(ca_cert_dir ^ "/foo.pem")
    ~priv_key:server_key >>= fun foocert ->

  let server_s =
    let open Lwt_unix in
    let s = socket PF_INET SOCK_STREAM 0 in
    bind s (ADDR_INET (Unix.inet_addr_any, port)) >|= fun () ->
    listen s 10 ;
    s in

  let handle ep channels addr =
    let host = match ep with
      | `Ok data -> ( match data.Tls.Core.own_name with
          | Some n -> n
          | None   -> "no name" )
      | `Error   -> "no session"
    in
    async @@ fun () ->
    Lwt.catch (fun () -> callback host channels addr >>= fun () -> yap ~tag "<- handler done")
      (function
        | Tls_lwt.Tls_alert a ->
          yap ~tag @@ "handler: " ^ Tls.Packet.alert_type_to_string a
        | exn -> yap ~tag "handler: exception" >>= fun () -> fail exn)
  in

  let ps = string_of_int port in
  yap ~tag ("-> start @ " ^ ps ^ " (use `openssl s_client -connect host:" ^ ps ^ " -servername foo` (or -servername bar))") >>= fun () ->
  let rec loop () =
    let config = Tls.Config.server ~certificates:(`Multiple [barcert ; foocert]) () in
    server_s >>= fun s ->
    Tls_lwt.Unix.accept config s >>= fun (t, addr) ->
    yap ~tag "-> connect" >>= fun () ->
    ( handle (Tls_lwt.Unix.epoch t) (Tls_lwt.of_t t) addr ; loop () )
  in
  loop ()


let echo_server port =
  Mirage_crypto_rng_lwt.initialize () >>= fun () ->
  serve_ssl port @@ fun host (ic, oc) _addr ->
    lines ic |> Lwt_stream.iter_s (fun line ->
      yap ~tag:("handler " ^ host) ("+ " ^ line) >>= fun () ->
      Lwt_io.write_line oc line)

let () =
  let port =
    try int_of_string Sys.argv.(1) with _ -> 4433
  in
  Lwt_main.run (echo_server port)
