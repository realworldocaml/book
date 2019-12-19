open! Base
open! Expect_test_helpers_base
open! Uopt
open! Optional_syntax

let%expect_test _ =
  print_s [%sexp (is_none none : bool)];
  [%expect {| true |}]
;;

let%expect_test _ =
  print_s [%sexp (is_some none : bool)];
  [%expect {| false |}]
;;

let%expect_test _ =
  print_s [%sexp (is_none (some 13) : bool)];
  [%expect {| false |}]
;;

let%expect_test _ =
  print_s [%sexp (is_some (some 13) : bool)];
  [%expect {| true |}]
;;

let%expect_test _ =
  require_does_raise [%here] (fun () -> value_exn none);
  [%expect {| (Failure Uopt.value_exn) |}]
;;

let%expect_test _ =
  print_s [%sexp (value_exn (some 13) : int)];
  [%expect {| 13 |}]
;;

let%expect_test _ =
  print_s [%sexp (unsafe_value (some 13) : int)];
  [%expect {| 13 |}]
;;

let%expect_test "[match%optional none]" =
  require
    [%here]
    (match%optional none with
     | None -> true
     | Some _ -> false)
;;

let%expect_test "[match%optional some]" =
  require
    [%here]
    (match%optional some 13 with
     | None -> false
     | Some x -> x = 13)
;;
