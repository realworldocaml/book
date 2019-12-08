let%expect_test _ =
  print_string "one";
  [%expect {| two |}];
;;
