open! Base
open! Stdio

let%expect_test _ =
  print_s [%sexp (List.rev [3;2;1] : int list)];
  [%expect {| (1 2 3) |}]
