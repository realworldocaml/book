open! Import

let%expect_test _ =
  let f x = x * 2 in
  let g x = x + 3 in
  print_s [%sexp (f @@ 5 : int)];
  [%expect {| 10 |}];
  print_s [%sexp (g @@ f @@ 5 : int)];
  [%expect {| 13 |}];
  print_s [%sexp (f @@ g @@ 5 : int)];
  [%expect {| 16 |}];
;;

let%expect_test "exp is present at the toplevel" =
  print_s [%sexp (2 ** 8 : int)];
  [%expect {| 256 |}]
;;
