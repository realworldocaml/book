(* Auto-generated from "test_abstract.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type int_assoc_list = Testj.int_assoc_list

type any_items = Test_abstract_t.any_items

type any = Test_abstract_t.any

type 'x abs2 = 'x Test.abs2

type 'x abs1 = 'x Test_abstract_t.abs1

val validate_int_assoc_list :
  Atdgen_runtime.Util.Validation.path -> int_assoc_list -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!type:int_assoc_list}. *)

val validate_any_items :
  Atdgen_runtime.Util.Validation.path -> any_items -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!type:any_items}. *)

val validate_any :
  Atdgen_runtime.Util.Validation.path -> any -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!type:any}. *)

val validate_abs2 :
  (Atdgen_runtime.Util.Validation.path -> 'x -> Atdgen_runtime.Util.Validation.error option) ->
  Atdgen_runtime.Util.Validation.path -> 'x abs2 -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!type:abs2}. *)

val validate_abs1 :
  (Atdgen_runtime.Util.Validation.path -> 'x -> Atdgen_runtime.Util.Validation.error option) ->
  Atdgen_runtime.Util.Validation.path -> 'x abs1 -> Atdgen_runtime.Util.Validation.error option
  (** Validate a value of type {!type:abs1}. *)

