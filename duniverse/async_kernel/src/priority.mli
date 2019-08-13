(** The priority of a job. *)

open! Core_kernel
open! Import

type t =
  | Normal
  | Low
[@@deriving sexp_of]

val normal : t
val low : t
