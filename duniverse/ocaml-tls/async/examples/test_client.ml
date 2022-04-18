open! Core
open! Async
open Deferred.Or_error.Let_syntax

let config = Tls.Config.client ~authenticator:(fun ?ip:_ ~host:_ _ -> Ok None) ()

let test_client () =
  let host = "127.0.0.1" in
  let port = 8443 in
  let hnp = Host_and_port.create ~host ~port in
  let%bind (_ : Tls_async.Session.t), rd, wr =
    (* we can't build a [[ `host ] Domain_name.t] from an IP address *)
    let host = None in
    Tls_async.connect config (Tcp.Where_to_connect.of_host_and_port hnp) ~host
  in
  let req =
    String.concat
      ~sep:"\r\n"
      [ "GET / HTTP/1.1"; "Host: " ^ host; "Connection: close"; ""; "" ]
  in
  Writer.write wr req;
  let%bind () = Writer.flushed wr |> Deferred.ok in
  let%bind () =
    match%map Reader.read_line rd |> Deferred.ok with
    | `Ok str -> print_endline str
    | `Eof -> print_endline "Eof reached"
  in
  Writer.close wr |> Deferred.ok
;;

let cmd = Command.async_or_error ~summary:"test client" (Command.Param.return test_client)
let () = Command_unix.run cmd
