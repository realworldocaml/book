open! Core
open! Async

let launch_echo_server ~port =
  Process.create_exn
    ~prog:"../bin/echo.exe"
    ~args:["-port";Int.to_string port;"-uppercase"]
    ()

let connect ~port =
  let%map (_sock,r,w) =
    Tcp.connect
      (Tcp.Where_to_connect.of_host_and_port {host="localhost";port})
  in
  (r,w)

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

let%expect_test "test uppercase echo" =
  let port = 8081 in
  let%bind process  = launch_echo_server ~port in
  Monitor.protect (fun () ->
      let%bind (r,w) = connect ~port in
      let%bind () = send_data r w "one two three\n" in
      let%bind () = [%expect] in
      let%bind () = send_data r w "one 2 three\n" in
      let%bind () = [%expect] in
      return ())
    ~finally:(fun () -> cleanup process)
