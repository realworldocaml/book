open Lwt
open Ex_common

let string_of_unix_err err f p =
  Printf.sprintf "Unix_error (%s, %s, %s)"
    (Unix.error_message err) f p

let serve_ssl port callback =

  let tag = "server" in

  X509_lwt.private_of_pems
    ~cert:server_cert
    ~priv_key:server_key >>= fun cert ->

  let server_s () =
    let open Lwt_unix in
    let s = socket PF_INET SOCK_STREAM 0 in
    setsockopt s SO_REUSEADDR true ;
    bind s (ADDR_INET (Unix.inet_addr_any, port)) >|= fun () ->
    listen s 10 ;
    s in

  let handle channels addr =
    async @@ fun () ->
      Lwt.catch (fun () -> callback channels addr >>= fun () -> yap ~tag "<- handler done")
        (function
          | Tls_lwt.Tls_alert a ->
            yap ~tag @@ "handler: " ^ Tls.Packet.alert_type_to_string a
          | Tls_lwt.Tls_failure a ->
            yap ~tag @@ "handler: " ^ Tls.Engine.string_of_failure a
          | Unix.Unix_error (e, f, p) ->
            yap ~tag @@ "handler: " ^ (string_of_unix_err e f p)
          | _exn -> yap ~tag "handler: exception")
  in

  yap ~tag ("-> start @ " ^ string_of_int port) >>= fun () ->
  let rec loop s =
    let authenticator = null_auth in
    let config = Tls.Config.server ~reneg:true ~certificates:(`Single cert) ~authenticator () in
    (Lwt.catch
       (fun () -> Tls_lwt.accept_ext config s >|= fun r -> `R r)
       (function
         | Unix.Unix_error (e, f, p) -> return (`L (string_of_unix_err e f p))
         | Tls_lwt.Tls_alert a -> return (`L (Tls.Packet.alert_type_to_string a))
         | Tls_lwt.Tls_failure f -> return (`L (Tls.Engine.string_of_failure f))
         | _exn -> return (`L "loop: exception"))) >>= function
    | `R (channels, addr) ->
      yap ~tag "-> connect" >>= fun () -> ( handle channels addr ; loop s )
    | `L (msg) ->
      yap ~tag ("server socket: " ^ msg) >>= fun () -> loop s
    in
    server_s () >>= fun s ->
    loop s

let echo_server _ port =
  Lwt_main.run (
    Mirage_crypto_rng_lwt.initialize () >>= fun () ->
    serve_ssl port @@ fun (ic, oc) _addr ->
    lines ic |> Lwt_stream.iter_s (fun line ->
        yap ~tag:"handler" ("+ " ^ line) >>= fun () ->
        Lwt_io.write_line oc line))

open Cmdliner

let port =
  let doc = "Port to connect to" in
  Arg.(value & opt int 4433 & info [ "port" ] ~doc)

let cmd =
  Term.(ret (const echo_server $ setup_log $ port)),
  Term.info "server" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
