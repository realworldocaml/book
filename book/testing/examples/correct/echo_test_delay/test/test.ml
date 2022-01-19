open! Core
open Async
open Helpers

let%expect_test "test uppercase echo" =
  let port = 8081 in
  let%bind process  = launch ~port ~uppercase:true in
  Monitor.protect (fun () ->
      let%bind () = Clock.after (Time.Span.of_sec 1.) in
      let%bind (r,w) = connect ~port in
      let%bind () = send_data r w "one two three\n" in
      let%bind () = [%expect{| ONE TWO THREE |}] in
      let%bind () = send_data r w "one 2 three\n" in
      let%bind () = [%expect{| ONE 2 THREE |}] in
      return ())
    ~finally:(fun () -> cleanup process)
