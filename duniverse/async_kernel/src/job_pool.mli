open! Core_kernel
open! Import

type slots = (Execution_context.t, Obj.t -> unit, Obj.t) Pool.Slots.t3
[@@deriving sexp_of]

type t = slots Pool.t [@@deriving sexp_of]

include Invariant.S with type t := t

val create : unit -> t
