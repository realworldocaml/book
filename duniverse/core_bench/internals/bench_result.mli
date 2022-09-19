(** A module internal to [Core_bench]. Please look at {!Bench}. *)
open! Core

type t

module Predictor_result : sig
  type t

  val ci95 : t -> Analysis_result.Ci95.t option
end

val nanos_est : t -> float
val cycles_est : t -> float
