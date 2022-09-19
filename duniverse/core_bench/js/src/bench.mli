open! Core
open! Core_bench_internals
module Quota = Quota
module Test = Test
module Run_config = Run_config
module Display_column = Display_column
module Display_config = Display_config
module Variable = Variable
module Measurement = Measurement
module Analysis_config = Analysis_config
module Analysis_result = Analysis_result

(** [bench tests] will run, analyze and display the specified [tests]. Use this when one
    needs more control over the execution parameters that what is exposed through
    [make_command]. [bench] can also save the measurements of each test to the filename
    returned by [save_to_file]. *)
val bench
  :  ?run_config:Run_config.t
  -> ?analysis_configs:Analysis_config.t list
  -> ?display_config:Display_config.t
  -> ?save_to_file:(Measurement.t -> string)
  -> ?libname:string
  -> Test.t list
  -> unit

(** [measure] is a fragment of the functionality of [bench]. [measure tests] will run
    the specified [tests] and return the resulting measurement results. *)
val measure : ?run_config:Run_config.t -> Test.t list -> Measurement.t list

(** [analyze] is a fragment of the functionality of [bench]. [analyze ~analysis_configs m]
    will analyze the measurement [m] using the regressions specified. *)
val analyze
  :  ?analysis_configs:Analysis_config.t list
  -> Measurement.t
  -> Analysis_result.t Or_error.t

(** [display] is a fragment of the functionality of [bench]. [display results] will
    display a tabular summary of [results] on the terminal. *)
val display
  :  ?libname:string
  -> ?display_config:Display_config.t
  -> Analysis_result.t list
  -> unit
