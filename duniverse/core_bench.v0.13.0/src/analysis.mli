(** A module internal to [Core_bench]. Please look at {!Bench}.

   Does the analysis of collected measurements. *)

open! Core

val analyze
  :  Measurement.t
  -> Analysis_config.t list
  -> Analysis_result.t Or_error.t



