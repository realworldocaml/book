open Lwt
open Ex_common

let string_of_unix_err err f p =
  Printf.sprintf "Unix_error (%s, %s, %s)"
    (Unix.error_message err) f p

let add_to_cache, find_in_cache =
  let c = ref [] in
  (fun ticket session ->
     let id = ticket.Tls.Core.identifier in
     Logs.info (fun m -> m "adding id %a to cache" Cstruct.hexdump_pp id) ;
     c := (id, (ticket, session)) :: !c),
  (fun id -> match List.find_opt (fun (id', _) -> Cstruct.compare id id' = 0) !c with
     | None -> None
     | Some (_, ep) -> Some ep)

let ticket_cache = {
  Tls.Config.lookup = find_in_cache ;
  ticket_granted = add_to_cache ;
  lifetime = 300l ;
  timestamp = Ptime_clock.now
}

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
    let config = Tls.Config.server ~ticket_cache ~reneg:true ~certificates:(`Single cert) ~version:(`TLS_1_2, `TLS_1_3) ~zero_rtt:32768l () in
    (Lwt.catch
       (fun () -> Tls_lwt.Unix.accept config s >|= fun r -> `R r)
       (function
         | Unix.Unix_error (e, f, p) -> return (`L (string_of_unix_err e f p))
         | Tls_lwt.Tls_alert a -> return (`L (Tls.Packet.alert_type_to_string a))
         | Tls_lwt.Tls_failure f -> return (`L (Tls.Engine.string_of_failure f))
         | exn -> let str = Printexc.to_string exn in return (`L ("loop: exception " ^ str)))) >>= function
    | `R (t, addr) ->
      let channels = Tls_lwt.of_t t in
      yap ~tag "-> connect" >>= fun () -> ( handle channels addr ; loop s )
    | `L (msg) ->
      yap ~tag ("server socket: " ^ msg) >>= fun () -> loop s
    in
    server_s () >>= fun s ->
    loop s

let echo_server port =
  Mirage_crypto_rng_lwt.initialize () >>= fun () ->
  serve_ssl port @@ fun (ic, oc) _addr ->
    yap ~tag:"handler" "accepted" >>= fun () ->
    let out = "HTTP/1.1 404 Not Found\r\n\r\n" in
    Lwt_io.write_from_string_exactly oc out 0 (String.length out) >>= fun () ->
    (*    Lwt_io.close oc *)
    let rec loop () =
      Lwt_io.read_line ic >>= fun line ->
      yap ~tag:"handler" ("+ " ^ line) >>= fun () ->
      loop ()
    in
    loop ()

let jump _ port =
  Lwt_main.run (echo_server port)

open Cmdliner

let port =
  let doc = "Port to connect to" in
  Arg.(value & opt int 4433 & info [ "port" ] ~doc)

let cmd =
  Term.(ret (const jump $ setup_log $ port)),
  Term.info "server" ~version:"%%VERSION_NUM%%"

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
