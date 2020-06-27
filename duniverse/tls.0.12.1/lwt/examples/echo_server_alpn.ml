
open Lwt
open Ex_common

let split_on_char sep s =
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if s.[i] = sep then begin
      r := String.sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  String.sub s 0 !j :: !r

let serve_ssl alpn_protocols port callback =

  let tag = "server" in

  X509_lwt.private_of_pems
    ~cert:server_cert
    ~priv_key:server_key >>= fun certificate ->

  let server_s =
    let open Lwt_unix in
    let s = socket PF_INET SOCK_STREAM 0 in
    bind s (ADDR_INET (Unix.inet_addr_any, port)) >|= fun () ->
    listen s 10 ;
    s in

  let handle ep channels addr =
    let alpn = match ep with
      | `Ok data -> (match data.Tls.Core.alpn_protocol with
          | Some a -> a
          | None   -> "no alpn")
      | `Error   -> "no session"
    in
    async @@ fun () ->
    Lwt.catch (fun () -> callback alpn channels addr >>= fun () -> yap ~tag "<- handler done")
      (function
        | Tls_lwt.Tls_alert a ->
          yap ~tag @@ "handler: " ^ Tls.Packet.alert_type_to_string a
        | exn -> yap ~tag "handler: exception" >>= fun () -> fail exn)
  in

  let ps = string_of_int port in
  yap ~tag ("-> start @ " ^ ps ^ " (use `openssl s_client -connect host:" ^ ps ^ " -alpn <proto>`), available protocols: " ^ String.concat "," alpn_protocols) >>= fun () ->
  let rec loop () =
    let config = Tls.Config.server ~certificates:(`Single certificate) ~alpn_protocols () in
    server_s >>= fun s ->
    Tls_lwt.Unix.accept config s >>= fun (t, addr) ->
    yap ~tag "-> connect" >>= fun () ->
    ( handle (Tls_lwt.Unix.epoch t) (Tls_lwt.of_t t) addr ; loop () )
  in
  loop ()


let echo_server protocols port =
  Mirage_crypto_rng_lwt.initialize () >>= fun () ->
  serve_ssl protocols port @@ fun alpn (ic, oc) _addr ->
    lines ic |> Lwt_stream.iter_s (fun line ->
      yap ~tag:("handler alpn: " ^ alpn) ("+ " ^ line) >>= fun () ->
      Lwt_io.write_line oc line)

let () =
  let protocols =
    try split_on_char ',' Sys.argv.(1) with _ -> [ "h2" ; "http/1.1" ]
  in
  Lwt_main.run (echo_server protocols 4433)
