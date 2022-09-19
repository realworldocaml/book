(** A module internal to [Core_bench]. Please look at {!Bench}. *)
open! Core

type t =
  [ `Name
  | `Speedup
  | `Percentage
  | `Samples
  ]
[@@deriving equal, compare, sexp]
