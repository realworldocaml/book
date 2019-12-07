
(* The test framework should be resilient to user changing the current directory. *)

let%expect_test _ =
  print_string "hello world\n";
  Unix.chdir "..";
  [%expect {|
  hello world
|}]
