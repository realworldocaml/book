let%expect_test _ =
  print_endline "foo";
  [%expect {|foo|}];
  print_endline "Something went horribly wrong, exiting prematurely!";
  (exit 42 : unit);
  [%expect {| random output |}];
;;
