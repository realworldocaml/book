open! Core

let%expect_test _ =
  [%expect {| hi ho |}];
  Printexc.record_backtrace false;
  ignore (failwith "hi ho" : unit);
  [%expect {| it's off to work we go |}]
;;
