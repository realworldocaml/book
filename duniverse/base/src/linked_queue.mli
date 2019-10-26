(** This module is a Base-style wrapper around OCaml's standard [Queue] module. *)

open! Import

include Queue_intf.S with type 'a t = 'a Caml.Queue.t (** @inline *)

(** [create ()] returns an empty queue. *)
val create : unit -> _ t

(** [transfer ~src ~dst] adds all of the elements of [src] to the end of [dst], then
    clears [src].  It is equivalent to the sequence:

    {[
      iter ~src ~f:(enqueue dst);
      clear src
    ]}

    but runs in constant time. *)
val transfer : src:'a t -> dst:'a t -> unit
