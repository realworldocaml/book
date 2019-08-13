open Core

let%expect_test _ =
  print_string "alas, poor Yorick";
  [%expect {| alas, poor Yorick (escaped) |}]
;;

let%expect_test _ =
  print_string "hello world";
  [%expect {| hello\032world (escaped) |}]
;;

let%expect_test _ =
  print_string "hello\tworld";
  [%expect {| hello\tworld (escaped) |}]
;;

(*let%expect_test _ =
  print_string "hello\tworld";
  [%expect {| hello\tworld (regexp) |}]
  ;; *)
