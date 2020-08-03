(** A module internal to [Core_bench]. Please look at {!Bench}.

   Generates the command line interface to [Core_bench]. *)

open! Core

type callback_bench
  =  ?run_config:Run_config.t
  -> ?analysis_configs:Analysis_config.t list
  -> ?display_config:Display_config.t
  -> ?save_to_file:(Measurement.t -> string)
  -> ?libname:string
  -> Test.t list
  -> unit

type callback_load_analyze_and_display
  = filenames:string list
  -> ?analysis_configs:Analysis_config.t list
  -> ?display_config:Display_config.t
  -> unit
  -> unit

val make
  :  bench:callback_bench
  -> analyze:callback_load_analyze_and_display
  -> tests:Test.t list
  -> Command.t

val make_ext
  :  summary:string
  -> (Analysis_config.t list * Display_config.t *
      [ `From_file of string list
      | `Run of (Measurement.t -> string) option * Run_config.t ]
      -> unit)
     Command.Param.t
  -> Command.t
