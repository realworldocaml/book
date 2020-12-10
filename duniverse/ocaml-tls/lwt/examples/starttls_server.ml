open Lwt.Infix

let capability = "[CAPABILITY IMAP4rev1 LITERAL+ SASL-IR LOGIN-REFERRALS ID ENABLE IDLE STARTTLS AUTH=PLAIN] server ready.\r\n"

let ok_starttls = "OK STARTTLS\r\n"

let cert () =
  X509_lwt.private_of_pems
  ~cert:"./certificates/server.pem"
  ~priv_key:"./certificates/server.key"

let init_socket addr port =
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_of_string addr, port) in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  Lwt_unix.bind socket sockaddr >|= fun () ->
  socket

let create_srv_socket addr port =
  init_socket addr port >|= fun socket ->
  Lwt_unix.listen socket 10;
  socket

let accept sock =
  Lwt_unix.accept sock >>= fun (sock_cl, addr) ->
  let ic = Lwt_io.of_fd ~close:(fun () -> Lwt.return_unit) ~mode:Lwt_io.input sock_cl in
  let oc = Lwt_io.of_fd ~close:(fun () -> Lwt.return_unit) ~mode:Lwt_io.output sock_cl in
  Lwt.return ((ic,oc), addr, sock_cl)

let start_server () =
  Mirage_crypto_rng_lwt.initialize () >>= fun () ->
  let write oc buff =
    Lwt_io.write oc buff >>= fun () -> Lwt_io.flush oc
  in
  let read ic =
    Lwt_io.read ic ~count:2048 >>= fun buff ->
    Printf.printf "%s%!" buff;
    Lwt.return buff
  in
  let parse buff =
    match String.index buff ' ' with
    | exception Not_found -> "", ""
    | idx ->
      let l = String.length buff in
      String.sub buff 0 idx, String.sub buff (succ idx) (l - succ idx)
  in
  let rec wait_cmd sock_cl ic oc =
    read ic >>= fun buff ->
    let tag,cmd = parse buff in
    match cmd with
    | "CAPABILITY" ->
      write oc ("* " ^ capability ^ tag ^ " OK CAPABILITY\r\n") >>= fun () ->
      wait_cmd sock_cl ic oc
    | "STARTTLS" ->
      write oc (tag ^ ok_starttls) >>= fun () ->
      Lwt_io.close ic >>= fun () ->
      Lwt_io.close oc >>= fun () ->
      cert () >>= fun cert ->
      Tls_lwt.Unix.server_of_fd
        (Tls.Config.server ~certificates:(`Single cert) ()) sock_cl >>= fun s ->
      let ic,oc = Tls_lwt.of_t s in
      write oc ("* OK " ^ capability) >>= fun () ->
      wait_cmd sock_cl ic oc
    | _ ->
      write oc ("BAD\r\n") >>= fun () ->
      wait_cmd sock_cl ic oc
  in
  create_srv_socket "127.0.0.1" 143 >>= fun sock ->
  accept sock >>= fun ((ic,oc), _addr, sock_cl) ->
  write oc ("* OK " ^ capability) >>= fun () ->
  wait_cmd sock_cl ic oc

let () =
  Lwt_main.run (start_server ())
