open! Core_kernel

(* build_info is not available when building tests.
   This test makes sure that one can parse the default value of build_info *)
let%test_unit _ = assert (String.( = ) Version_util.ocaml_version "")

open Version_util

let%expect_test "Version.parse" =
  print_s [%sexp (Version.parse "ssh://somerepo_04ce83e21002" : Version.t Or_error.t)];
  [%expect {| (Ok ((repo ssh://somerepo) (version 04ce83e21002))) |}]
;;

let%expect_test "backwards-compatible printing of rev40s" =
  let print s =
    print_s [%sexp (Version_util.For_tests.parse_generated_hg_version s : string list)]
  in
  (* old jenga: rev12 with underscores *)
  print "ssh://repo1_0123456789ab\nssh://repo2_04ce83e21002+\n";
  [%expect {| (ssh://repo1_0123456789ab ssh://repo2_04ce83e21002+) |}];
  (* new jenga: rev40 with spaces *)
  print
    "ssh://repo1 a123456789b123456789c123456789d123456789+\n\
     ssh://repo2 a123456789b123456789c123456789d123456789\n";
  [%expect {|
    (ssh://repo1_a123456789b1+ ssh://repo2_a123456789b1) |}];
  (* potentially in the future, when we change the hashing scheme: rev64 *)
  print "ssh://repo1 a1234567b1234567c1234567d1234567e1234567f1234567g1234567h1234567+\n";
  [%expect {| (ssh://repo1_a1234567b123+) |}]
;;
