open! Import
module Printexc = Caml.Printexc

let%expect_test "Printexc: built-in exception" =
  print_endline (Printexc.to_string (Invalid_argument "bad"));
  [%expect {| Invalid_argument("bad") |}]
;;

let%expect_test "Sexp conversion of built-in exceptions" =
  print_endline (Sexp.to_string (sexp_of_exn (Invalid_argument "bad")));
  [%expect {| (Invalid_argument bad) |}]
;;

exception My_invalid_argument of string [@@deriving sexp]

let%expect_test "Printexc: an exception with deriving sexp" =
  print_endline (Printexc.to_string (My_invalid_argument "bad"));
  [%expect {| (test_printexc.ml.My_invalid_argument bad) |}]
;;
