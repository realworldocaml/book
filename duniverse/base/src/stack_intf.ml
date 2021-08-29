(** An interface for stacks that follows [Core]'s conventions, as opposed to OCaml's
    standard [Stack] module. *)

open! Import

module type S = sig
  type 'a t [@@deriving_inline sexp]

  include Ppx_sexp_conv_lib.Sexpable.S1 with type 'a t := 'a t

  [@@@end]

  include Invariant.S1 with type 'a t := 'a t


  (** [fold], [iter], [find], and [find_map] visit the elements in order from the top of
      the stack to the bottom.  [to_list] and [to_array] return the elements in order from
      the top of the stack to the bottom.

      Iteration functions ([iter], [fold], etc.) have unspecified behavior (although they
      should still be memory-safe) when the stack is mutated while they are running (e.g.
      by having the passed-in function call [push] or [pop] on the stack).
  *)
  include
    Container.S1 with type 'a t := 'a t

  (** [of_list l] returns a stack whose top is the first element of [l] and bottom is the
      last element of [l]. *)
  val of_list : 'a list -> 'a t

  (** [create ()] returns an empty stack. *)
  val create : unit -> _ t

  (** [singleton a] creates a new stack containing only [a]. *)
  val singleton : 'a -> 'a t

  (** [push t a] adds [a] to the top of stack [t]. *)
  val push : 'a t -> 'a -> unit

  (** [pop t] removes and returns the top element of [t] as [Some a], or returns [None] if
      [t] is empty. *)
  val pop : 'a t -> 'a option

  val pop_exn : 'a t -> 'a

  (** [top t] returns [Some a], where [a] is the top of [t], unless [is_empty t], in which
      case [top] returns [None]. *)
  val top : 'a t -> 'a option

  val top_exn : 'a t -> 'a

  (** [clear t] discards all elements from [t]. *)
  val clear : _ t -> unit

  (** [copy t] returns a copy of [t]. *)
  val copy : 'a t -> 'a t

  (** [until_empty t f] repeatedly pops an element [a] off of [t] and runs [f a], until
      [t] becomes empty.  It is fine if [f] adds more elements to [t], in which case the
      most-recently-added element will be processed next. *)
  val until_empty : 'a t -> ('a -> unit) -> unit
end

(** A stack implemented with an array.

    The implementation will grow the array as necessary, and will never automatically
    shrink the array. One can use [set_capacity] to explicitly resize the array. *)
module type Stack = sig
  module type S = S

  include S (** @open *)

  (** [capacity t] returns the length of the array backing [t]. *)
  val capacity : _ t -> int

  (** [set_capacity t capacity] sets the length of the array backing [t] to [max capacity
      (length t)].  To shrink as much as possible, do [set_capacity t 0]. *)
  val set_capacity : _ t -> int -> unit
end
