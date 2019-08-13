(** A weak pointer is a pointer to a heap block that does not cause the heap block to
    remain live during garbage collection.

    If the block would otherwise remain live, then the weak pointer remains pointed
    to the block.  If the block is collected, then the weak pointer is cleared. *)

open! Core_kernel

type 'a t [@@deriving sexp_of]

(** [create] creates an empty weak pointer.  One must [set] it to point it to
    something. *)
val create : unit -> _ t

val get : 'a t -> 'a Heap_block.t option

(** [is_some t = Option.is_some (get t)]. *)
val is_some : _ t -> bool

(** [is_none t = Option.is_none (get t)]. *)
val is_none : _ t -> bool

val set : 'a t -> 'a Heap_block.t -> unit
