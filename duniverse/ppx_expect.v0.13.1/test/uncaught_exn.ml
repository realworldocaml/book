let%expect_test _ =
  Printexc.record_backtrace false;
  assert false
[@@expect.uncaught_exn {|
  "Assert_failure uncaught_exn.ml:3:2" |}]
;;

let%expect_test "Expectation with uncaught expectation" =
  Printexc.record_backtrace false;
  ignore (assert false);
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  "Assert_failure uncaught_exn.ml:10:9" |}]
;;
