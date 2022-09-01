open! Core

(* build_info is not available when building tests.
   This test makes sure that one can parse the default value of build_info *)
let%test_unit _ = assert (String.( = ) Version_util.ocaml_version "")

open Version_util

let%expect_test "Version.parse*" =
  let line1 = "ssh://repo_1_04ce83e21002" in
  let line2 = "ssh://repo_2_abcdefabcdfe+" in
  let no_version_util = "NO_VERSION_UTIL" in
  print_s [%sexp (Version.parse1 line1 : Version.t Or_error.t)];
  [%expect {| (Ok ((repo ssh://repo_1) (version 04ce83e21002))) |}];
  print_s [%sexp (Version.parse1 no_version_util : Version.t Or_error.t)];
  [%expect {| (Ok ((repo NO_VERSION) (version UTIL))) |}];
  print_s [%sexp (Version.parse_list [ line1; line2 ] : Version.t list option Or_error.t)];
  [%expect
    {|
    (Ok
     ((((repo ssh://repo_1) (version 04ce83e21002))
       ((repo ssh://repo_2) (version abcdefabcdfe+))))) |}];
  print_s
    [%sexp (Version.parse_list [ no_version_util ] : Version.t list option Or_error.t)];
  [%expect {| (Ok ()) |}];
  print_s
    [%sexp
      (Version.parse_list [ line1; no_version_util ] : Version.t list option Or_error.t)];
  [%expect {| (Ok ()) |}];
  print_s
    [%sexp
      (Version.parse_lines (line1 ^ "\n" ^ line2 ^ "\n")
       : Version.t list option Or_error.t)];
  [%expect
    {|
    (Ok
     ((((repo ssh://repo_1) (version 04ce83e21002))
       ((repo ssh://repo_2) (version abcdefabcdfe+))))) |}];
  print_s
    [%sexp
      (Version.parse_lines (no_version_util ^ "\n") : Version.t list option Or_error.t)];
  [%expect {| (Ok ()) |}];
  ignore (Version.current_version () : _ option (* check that it doesn't raise *));
  ()
;;

let%expect_test "backwards-compatible printing of rev40s" =
  let print s =
    print_s [%sexp (Version_util.For_tests.parse_generated_hg_version s : string list)]
  in
  (* current jenga *)
  print
    "ssh://repo1 a123456789b123456789c123456789d123456789+\n\
     ssh://repo2 a123456789b123456789c123456789d123456789\n";
  [%expect {|
    (ssh://repo1_a123456789b1+ ssh://repo2_a123456789b1) |}];
  (* potentially in the future, when we change the hashing scheme: rev64 *)
  print "ssh://repo1 a1234567b1234567c1234567d1234567e1234567f1234567g1234567h1234567+\n";
  [%expect {| (ssh://repo1_a1234567b123+) |}]
;;
