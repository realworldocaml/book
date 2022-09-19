(** A module internal to [Core_bench]. Please look at {!Bench}. *)
open! Core

val float_opt_to_string : float option -> string

val float_to_string
  :  ?delimiter:char
  -> ?strip_zero:bool
  -> ?explicit_plus:bool
  -> float
  -> string
