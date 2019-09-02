(** A [Balanced_reducer.t] stores a mutable fixed-length sequence of optional values, and
    incrementally maintains the result of folding an associative operation ([reduce]) over
    the sequence as its elements change. *)

open! Base

type 'a t [@@deriving sexp_of]

include Invariant.S1 with type 'a t := 'a t

(** [create_exn ~len ~reduce] creates a balanced reducer of length [len], all of whose
    elements are [None].  It raises if [len < 1]. *)
val create_exn
  :  ?sexp_of_a:('a -> Sexp.t) (** for improved error messages *)
  -> unit
  -> len:int
  -> reduce:('a -> 'a -> 'a)
  -> 'a t

(** [set_exn t i a] updates the value at index [i] to [Some a].  It raises if [i] is out
    of bounds. *)
val set_exn : 'a t -> int -> 'a -> unit

(** [get_exn t i] gets the value at index [i].  It raises if [i] is out of bounds, or
    [set_exn t i] has never been called. *)
val get_exn : 'a t -> int -> 'a

(** [compute_exn t] computes the value of the fold.  It raises if any values of the array
    are [None]. *)
val compute_exn : 'a t -> 'a
