open Core
open Async

let launch ~port ~uppercase =
  Process.create_exn
    ~prog:"../bin/echo.exe"
    ~args:
      ([ "-port"; Int.to_string port ]
      @ if uppercase then [ "-uppercase" ] else [])
    ()

let connect ~port =
  let%map _sock, r, w =
    Tcp.connect
      (Tcp.Where_to_connect.of_host_and_port
         { host = "localhost"; port })
  in
  r, w

let send_data r w text =
  Writer.write w text;
  let%bind () = Writer.flushed w in
  let%bind line = Reader.read_line r in
  (match line with
  | `Eof -> print_endline "EOF"
  | `Ok line -> print_endline line);
  return ()

let cleanup process =
  let () = Process.send_signal process Signal.kill in
  let%bind (_ : Unix.Exit_or_signal.t) = Process.wait process in
  return ()
