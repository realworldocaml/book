(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** @since 5.5.0 *)

type 'a t = unit -> 'a node Lwt.t
(** The type of delayed lists containing elements of type ['a].
  Note that the concrete list node ['a node] is delayed under a closure,
  not a [lazy] block, which means it might be recomputed every time
  we access it. *)

and +'a node = Nil | Cons of 'a * 'a t
(** A fully-evaluated list node, either empty or containing an element
  and a delayed tail. *)

val empty : 'a t
(** The empty sequence, containing no elements. *)

val return : 'a -> 'a t
(** The singleton sequence containing only the given element. *)

val return_lwt : 'a Lwt.t -> 'a t
(** The singleton sequence containing only the given promised element. *)

val cons : 'a -> 'a t -> 'a t
(** [cons x xs] is the sequence containing the element [x] followed by
  the sequence [xs] *)

val cons_lwt : 'a Lwt.t -> 'a t -> 'a t
(** [cons x xs] is the sequence containing the element promised by [x] followed
  by the sequence [xs] *)

val append : 'a t -> 'a t -> 'a t
(** [append xs ys] is the sequence [xs] followed by the sequence [ys] *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f seq] returns a new sequence whose elements are the elements of
  [seq], transformed by [f].
  This transformation is lazy, it only applies when the result is traversed. *)

val map_s : ('a -> 'b Lwt.t) -> 'a t -> 'b t
(** [map_s f seq] is like [map f seq] but [f] is a function that returns a
  promise.

  Note that there is no concurrency between the promises from the underlying
  sequence [seq] and the promises from applying the function [f]. In other
  words, the next promise-element of the underlying sequence ([seq]) is only
  created when the current promise-element of the returned sequence (as mapped
  by [f]) has resolved. This scheduling is true for all the [_s] functions of
  this module. *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** Remove from the sequence the elements that do not satisfy the
  given predicate.
  This transformation is lazy, it only applies when the result is
  traversed. *)

val filter_s : ('a -> bool Lwt.t) -> 'a t -> 'a t
(** [filter_s] is like [filter] but the predicate returns a promise.

  See {!map_s} for additional details about scheduling. *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** Apply the function to every element; if [f x = None] then [x] is dropped;
  if [f x = Some y] then [y] is returned.
  This transformation is lazy, it only applies when the result is
  traversed. *)

val filter_map_s : ('a -> 'b option Lwt.t) -> 'a t -> 'b t
(** [filter_map_s] is like [filter] but the predicate returns a promise.

  See {!map_s} for additional details about scheduling. *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** Map each element to a subsequence, then return each element of this
  sub-sequence in turn.
  This transformation is lazy, it only applies when the result is
  traversed. *)

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a Lwt.t
(** Traverse the sequence from left to right, combining each element with the
  accumulator using the given function.
  The traversal happens immediately and will not terminate (i.e., the promise
  will not resolve) on infinite sequences. *)

val fold_left_s : ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b t -> 'a Lwt.t
(** [fold_left_s] is like [fold_left] but the function returns a promise.

  See {!map_s} for additional details about scheduling. *)

val iter : ('a -> unit) -> 'a t -> unit Lwt.t
(** Iterate on the sequence, calling the (imperative) function on every element.

  The sequence's next node is evaluated only once the function has finished
  processing the current element. More formally: the promise for the [n+1]th
  node of the sequence is created only once the promise returned by [f] on the
  [n]th element of the sequence has resolved.

  The traversal happens immediately and will not terminate (i.e., the promise
  will not resolve) on infinite sequences. *)

val iter_s : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t
(** [iter_s] is like [iter] but the function returns a promise.

  See {!map_s} for additional details about scheduling. *)

val iter_p : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t
(** Iterate on the sequence, calling the (imperative) function on every element.

  The sequence's next node is evaluated as soon as the previous node is
  resolved.

  The traversal happens immediately and will not terminate (i.e., the promise
  will not resolve) on infinite sequences. *)

val iter_n : ?max_concurrency:int -> ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t
(** [iter_n ~max_concurrency f s]

  Iterates on the sequence [s], calling the (imperative) function [f] on every
  element.

  The sum total of unresolved promises returned by [f] never exceeds
  [max_concurrency]. Node suspensions are evaluated only when there is capacity
  for [f]-promises to be evaluated. Consequently, there might be significantly
  fewer than [max_concurrency] promises being evaluated concurrently; especially
  if the node suspensions take longer to evaluate than the [f]-promises.

  The traversal happens immediately and will not terminate (i.e., the promise
  will not resolve) on infinite sequences.

  @param max_concurrency defaults to [1].
  @raise Invalid_argument if [max_concurrency < 1]. *)

val unfold : ('b -> ('a * 'b) option) -> 'b -> 'a t
(** Build a sequence from a step function and an initial value.
  [unfold f u] returns [empty] if the promise [f u] resolves to [None],
  or [fun () -> Lwt.return (Cons (x, unfold f y))] if the promise [f u] resolves
  to [Some (x, y)]. *)

val unfold_lwt : ('b -> ('a * 'b) option Lwt.t) -> 'b -> 'a t
(** [unfold_lwt] is like [unfold] but the step function returns a promise. *)

val to_list : 'a t -> 'a list Lwt.t
(** Convert a sequence to a list, preserving order.
  The traversal happens immediately and will not terminate (i.e., the promise
  will not resolve) on infinite sequences. *)

val of_list : 'a list -> 'a t
(** Convert a list to a sequence, preserving order. *)

val of_seq : 'a Seq.t -> 'a t
(** Convert from ['a Stdlib.Seq.t] to ['a Lwt_seq.t].
  This transformation is lazy, it only applies when the result is
  traversed. *)

val of_seq_lwt : 'a Lwt.t Seq.t -> 'a t
(** Convert from ['a Lwt.t Stdlib.Seq.t] to ['a Lwt_seq.t].
  This transformation is lazy, it only applies when the result is
  traversed. *)
