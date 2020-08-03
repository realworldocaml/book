(** Module for dealing with weak pointers, i.e., pointers that don't prevent garbage
    collection of what they point to.

    This module is like the OCaml standard library module of the same name, except that it
    requires that the values in the weak set are heap blocks. *)

open! Core_kernel

type 'a t [@@deriving sexp_of]

val create : len:int -> _ t
val length : _ t -> int
val set : 'a t -> int -> 'a Heap_block.t option -> unit

(** [set_exn] raises an exception if given [Some x] with [x] not being
    a heap block. This is in addition to raising exceptions on bounds violation as [set]
    does. *)
val set_exn : 'a t -> int -> 'a option -> unit
val get : 'a t -> int -> 'a Heap_block.t option
val is_some : _ t -> int -> bool
val is_none : _ t -> int -> bool

val iter : 'a t -> f:('a -> unit) -> unit
val iteri : 'a t -> f:(int -> 'a -> unit) -> unit

(**
   Warning! [blit] often takes time linear in the size of the arrays, not in the size of
   the range being copied. This issue is being tracked in
   https://github.com/ocaml/ocaml/issues/9258.

   Other than that, [blit] is generally preferred over [get] followed by [set]
   because, unlike [get], it doesn't have to make the value strongly-referenced.
   Making a value strongly-referenced, even temporarily, may result in delaying
   its garbage collection by a whole GC cycle.
*)
val blit : src:'a t -> src_pos:int -> dst:'a t -> dst_pos:int -> len:int -> unit
