open! Core

(** The purpose of functions in this module are documented in the [Core_bench.Bench] module

    These functions are different in one main regard: they take first-class functions
    for platform-specific functionality, like displaying the tables, or performing the
    actual measurement for the benchmark.  That is what these
    [measure_with] and [display] parameters are for. *)

val bench
  :  ?run_config:Run_config.t
  -> ?analysis_configs:Analysis_config.t list
  -> ?display_config:'a
  -> ?save_to_file:(Measurement.t -> string)
  -> ?libname:'b
  -> Test.t list
  -> measure_with:(Run_config.t -> Test.Basic_test.t list -> Measurement.t list)
  -> display:(?libname:'b -> ?display_config:'a -> Analysis_result.t list -> unit)
  -> unit

val measure
  :  ?run_config:Run_config.t
  -> Test.t list
  -> measure_with:(Run_config.t -> Test.Basic_test.t list -> Measurement.t list)
  -> Measurement.t list

val analyze
  :  ?analysis_configs:Analysis_config.t list
  -> Measurement.t
  -> Analysis_result.t Or_error.t

val analyze_and_display
  :  measurements:Measurement.t list
  -> ?analysis_configs:Analysis_config.t list
  -> ?display_config:Display_config.t
  -> ?libname:'b
  -> display:
       (?libname:'b -> ?display_config:Display_config.t -> Analysis_result.t list -> 'c)
  -> unit
  -> 'c
