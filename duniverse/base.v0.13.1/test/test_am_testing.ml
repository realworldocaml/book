open! Base
open! Import

let%expect_test _ =
  print_s [%sexp (Exported_for_specific_uses.am_testing : bool)];
  [%expect {|
    true |}]
;;
