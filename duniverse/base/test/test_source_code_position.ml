open! Base
open! Import

let%expect_test "[%here]" =
  print_s [%sexp [%here]];
  [%expect {| lib/base/test/test_source_code_position.ml:5:17 |}]
;;

let%expect_test "of_pos __POS__" =
  let here = Source_code_position.of_pos Caml.__POS__ in
  print_s [%sexp (here : Source_code_position.t)];
  [%expect {| test_source_code_position.ml:10:41 |}]
;;
