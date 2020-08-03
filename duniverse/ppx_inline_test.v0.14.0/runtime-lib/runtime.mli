
module Test_result : sig
  type t = Success | Failure | Error

  val combine : t -> t -> t
  val combine_all : t list -> t

  val to_string : t -> string
end

type config = (module Inline_test_config.S)
type 'a test_function_args
   = config:config
  -> descr:string
  -> tags:string list
  -> filename:string
  -> line_number:int
  -> start_pos:int
  -> end_pos:int
  -> 'a
val set_lib_and_partition : string -> string -> unit
val unset_lib : string -> unit
val test : ((unit -> bool) -> unit) test_function_args
val test_unit : ((unit -> unit) -> unit) test_function_args
val test_module : ((unit -> unit) -> unit) test_function_args


(** [`Am_test_runner] means the [./inline_tests_runner] process, whereas
    [`Am_child_of_test_runner] means a process descended from the test runner. *)
val testing : [ `Not_testing | `Testing of [ `Am_test_runner | `Am_child_of_test_runner ]]

val use_color : bool
val in_place : bool
val diff_command : string option
val source_tree_root : string option

(** Allow patterns in tests expectation *)
val allow_output_patterns : bool

(** [am_running_inline_test] is [true] if the code is running inline tests
    (e.g. [let%expect_test], [let%test], [let%test_unit]) or is in an executable
    invoked from inline tests. *)
val am_running_inline_test : bool
val am_running_inline_test_env_var : string

(** Record an evaluator for an external set of tests *)
val add_evaluator : f:(unit -> Test_result.t) -> unit

(** Exit with a status based on the combined result of all recorded evaluators *)
val exit : unit -> _
