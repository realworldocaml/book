(** [Ivar_filler] is a reference to an ivar that allows one to [fill] the ivar, but not to
    read it.  This allows the implementation to drop the reference to the ivar once it is
    full, which can be useful to avoid holding onto unused memory. *)

open! Core_kernel
open! Import

type 'a t [@@deriving sexp_of]

include Invariant.S1 with type 'a t := 'a t

val create : unit -> 'a t * 'a Deferred0.t
val is_empty : 'a t -> bool
val fill : 'a t -> 'a -> unit
