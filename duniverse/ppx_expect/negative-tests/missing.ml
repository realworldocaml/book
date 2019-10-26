open! Core

(* Example with no [%expect] node at all *)

let%expect_test _ =
  print_string "hello\n";
  print_string "goodbye\n"
