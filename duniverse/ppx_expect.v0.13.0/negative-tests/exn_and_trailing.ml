let%expect_test _ =
  print_endline "hello";
  if true then raise Exit;
  [%expect {| hello |}]
[@@expect.uncaught_exn {| Exit |}]
;;
