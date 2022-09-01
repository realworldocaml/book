(** A module internal to [Core_bench]. Please look at {!Bench}. *)
open! Core

type t =
  { regression_name : string option
  ; responder : Variable.t
  ; predictors : Variable.t list
  ; bootstrap_trials : int
  ; r_square : bool
  }

val allocations_vs_runs : t list

val create
  :  responder:Variable.t
  -> predictors:Variable.t list
  -> ?bootstrap_trials:int
  -> ?r_square:bool
  -> ?regression_name:string
  -> unit
  -> t

val cycles : predictors:Variable.t list -> t
val cycles_vs_runs : t
val default : t list
val default_bootstrap_trials : int
val default_reduced_bootstrap_trials : int
val gc_vs_runs : t list
val make_key : t -> int
val nanos : predictors:Variable.t list -> t
val nanos_vs_runs : t
val parse : ?regression_name:string -> string -> t
val reduce_bootstrap : t -> bootstrap_trials:int -> t
val with_error_estimation : ?bootstrap_trials:int -> t -> t
