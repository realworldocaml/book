open! Base
open! Stdio

let%expect_test "multi-block" =
  print_endline "Hello";
  [%expect{| Hello |}];
  print_endline "World!";
  [%expect{| World! |}]
