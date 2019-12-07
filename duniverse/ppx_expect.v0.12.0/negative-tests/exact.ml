open! Core

(* Check that [%expect_exact] does not strip leading/trailing newlines *)
let%expect_test _ =
  print_string "foobarbaz";
  [%expect_exact {|
  foobarbaz
  |}]

(* Check that [%expect_exact] does not treat whitespace as indentation *)
let%expect_test _ =
  print_string "\nfoobarbaz\n";
  [%expect_exact {|
    foobarbaz
  |}]

(* Check that [%expect_exact] does not strip whitespace on single lines *)
let%expect_test _ =
  print_string "foobarbaz";
  [%expect_exact {| foobarbaz |}]
