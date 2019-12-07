(** A write-once cell that can be empty or full (i.e., hold a single value).

    One can [read] an ivar to obtain a deferred that becomes determined when the ivar is
    filled.  An ivar is similar to an ['a option ref], except it is an error to fill an
    already full ivar. *)

open! Core_kernel
open! Import

type 'a t = 'a Types.Ivar.t [@@deriving bin_io, sexp_of]
type 'a ivar = 'a t

include Invariant.S1 with type 'a t := 'a t

(** [equal t t'] is physical equality of [t] and [t']. *)
val equal : 'a t -> 'a t -> bool

(** [create ()] returns an empty ivar. *)
val create : unit -> 'a t

(** [create_full v] returns an ivar filled with [v]. *)
val create_full : 'a -> 'a t

(** [fill t v] fills [t] with value [v] if [t] was empty.  If [t] was full, [fill] raises
    an exception.  It is guaranteed that immediately after calling [fill t], [is_some
    (Deferred.peek (read t))]. *)
val fill : 'a t -> 'a -> unit

(** [fill_if_empty t v] fills [t] with [v] if [t] is currently empty.  If [t] is full,
    then [fill_if_empty] does nothing.  *)
val fill_if_empty : 'a t -> 'a -> unit

(** [is_empty t] returns true if [t] is empty. *)
val is_empty : 'a t -> bool

(** [is_full t] returns true if [t] is full. *)
val is_full : 'a t -> bool

(** [read t] returns a deferred that becomes enabled with value [v] after the ivar is
    filled with [v]. *)
val read : 'a t -> 'a Deferred0.t

(** [peek t] returns [Some v] iff [t] is full with value [v]. *)
val peek : 'a t -> 'a option

(** [value_exn t] returns [v] if [t] is full with value [v], and raises otherwise. *)
val value_exn : 'a t -> 'a

(** [has_handlers t] returns [true] if [t] has handlers waiting on [read t]. *)
val has_handlers : _ t -> bool
