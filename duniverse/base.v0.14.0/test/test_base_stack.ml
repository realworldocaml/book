open! Import
open Stack
include Test_container.Test_S1 (Stack)
include Test_stack.Test (Test_stack.Debug (Stack))

let capacity = capacity
let set_capacity = set_capacity

let%test_unit _ =
  let t = create () in
  [%test_result: int] (capacity t) ~expect:0;
  set_capacity t (-1);
  [%test_result: int] (capacity t) ~expect:0;
  set_capacity t 10;
  [%test_result: int] (capacity t) ~expect:10;
  set_capacity t 0;
  [%test_result: int] (capacity t) ~expect:0;
  push t ();
  set_capacity t 0;
  [%test_result: int] (length t) ~expect:1;
  [%test_pred: int] (fun c -> c >= 1) (capacity t)
;;
