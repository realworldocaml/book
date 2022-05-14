open! Core
open! Import
open Patdiff_kernel.File_helpers

let%expect_test "lines_of_contents" =
  let test contents =
    print_s [%sexp (lines_of_contents contents : string array * Trailing_newline.t)]
  in
  test "";
  [%expect {| (() With_trailing_newline) |}];
  test "hello";
  [%expect {| ((hello) Missing_trailing_newline) |}];
  test "hello\nworld";
  [%expect {| ((hello world) Missing_trailing_newline) |}];
  test "hello\nworld\n";
  [%expect {| ((hello world) With_trailing_newline) |}]
;;
