open! Core
open Async
open Helpers

let%expect_test "test uppercase echo" =
  let port = 8081 in
  let%bind process = launch ~port ~uppercase:true in
  Monitor.protect
    (fun () ->
      let%bind r, w = connect ~port in
      let%bind () = send_data r w "one two three\n" in
      [%expect {| ONE TWO THREE |}];
      let%bind () = send_data r w "one 2 three\n" in
      [%expect {| ONE 2 THREE |}];
      return ())
    ~finally:(fun () -> cleanup process)
