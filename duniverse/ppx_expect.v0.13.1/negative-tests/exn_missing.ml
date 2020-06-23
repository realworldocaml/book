open! Core

let%expect_test "without trailing output" =
  printf "hello world";
  [%expect "hello world"]
[@@expect.uncaught_exn {| (Failure "hi ho") |}]
;;

let%expect_test "with trailing output" =
  printf "hello world"
[@@expect.uncaught_exn {| (Failure "hi ho") |}]
;;
