(** Module for dealing with weak pointers, i.e., pointers that don't prevent garbage
    collection of what they point to.

    This module is like the OCaml standard library module of the same name, except that it
    requires that the values in the weak set are heap blocks. *)

open! Core_kernel

type 'a t [@@deriving sexp_of]

val create : len:int -> _ t
val length : _ t -> int
val set : 'a t -> int -> 'a Heap_block.t option -> unit
val get : 'a t -> int -> 'a Heap_block.t option
val is_some : _ t -> int -> bool
val is_none : _ t -> int -> bool
