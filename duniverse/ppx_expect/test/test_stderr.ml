let%expect_test "stderr is collected" =
  Printf.eprintf "hello\n";
  [%expect {| hello |}];
;;
