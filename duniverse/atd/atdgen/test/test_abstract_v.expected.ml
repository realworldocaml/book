(* Auto-generated from "test_abstract.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type int_assoc_list = Testj.int_assoc_list

type any_items = Test_abstract_t.any_items

type any = Test_abstract_t.any

type 'x abs2 = 'x Test.abs2

type 'x abs1 = 'x Test_abstract_t.abs1

let validate_int_assoc_list = (
  Testj.validate_int_assoc_list
)
let validate__1 = (
  Atdgen_runtime.Ov_run.validate_list (
    (fun _ _ -> None)
  )
)
let validate_any_items = (
  validate__1
)
let validate_any = (
  (fun _ _ -> None)
)
let validate_abs2 validate__x = (
  Test.validate_abs2 validate__x
)
let validate_abs1 validate__x = (
  (fun _ _ -> None)
)
