open! Base
open! Import

let%expect_test "[Base.Hash.state] unifies with [Base_boot.Hash.state]" =
  let _f (state : Base.Hash.state) : Base_boot.Hash.state = state in
  [%expect {| |}]
;;
