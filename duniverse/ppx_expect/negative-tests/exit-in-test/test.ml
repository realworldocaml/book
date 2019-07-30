let%expect_test _ =
  ignore (Sys.command "./broken-test/inline_tests_runner" : int);
  [%expect {|
    File "test.ml", line 1, characters 0-187:
    Error: program exited while expect test was running!
    Output captured so far:
    foo
    Something went horribly wrong, exiting prematurely!
  |}]
;;
