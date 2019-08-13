open! Core

(* Example with trailing output after last [%expect] node *)

let%expect_test _ =
  print_string "hello";
  [%expect {| hello |}];

  print_string "goodbye\n"

let%expect_test _ =
  print_string "foo";
  [%expect {| foo |}];

  print_string "bar"
;;

let%expect_test _ =
  print_string "hello world";
  [%expect {| hello world |}]
