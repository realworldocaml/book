(** A module internal to [Core_bench]. Please look at {!Bench}. *)
open! Core

type t =
  { verbosity : Verbosity.t
  ; no_compactions : bool
  ; quota : Quota.t
  ; sampling_type : [ `Geometric of float | `Linear of int ]
  ; stabilize_gc_between_runs : bool
  ; fork_each_benchmark : bool
  ; thin_overhead : int option
  }
[@@deriving fields, sexp]

val create
  :  ?verbosity:Verbosity.t
  -> ?no_compactions:bool
  -> ?quota:Quota.t
  -> ?sampling_type:[ `Geometric of float | `Linear of int ]
  -> ?stabilize_gc_between_runs:bool
  -> ?fork_each_benchmark:bool
  -> ?thin_overhead:int
  -> unit
  -> t
