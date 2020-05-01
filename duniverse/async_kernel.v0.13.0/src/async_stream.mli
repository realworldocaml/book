(** An immutable sequence of values, with a possibly incomplete tail that may be extended
    asynchronously.

    For most applications one should use {!Pipe} instead of Stream.  One justifiable usage
    of [Stream] rather than [Pipe] is in single-writer, multi-consumer (multicast)
    scenarios where pushback is not required.

    The basic primitive operation for getting the next element out of stream is
    [Stream.next], which (asynchronously) returns the element and the rest of the
    stream. *)

open! Core_kernel
open! Import
module Deferred = Deferred1

(** [sexp_of_t t f] returns a sexp of all of the elements currently available in the
    stream.  It is just for display purposes.  There is no [t_of_sexp]. *)
type 'a t = 'a Tail.Stream.t [@@deriving sexp_of]

(** [create f] returns a stream [t] and calls [f tail], where the elements of the stream
    are determined as the tail is extended, and the end of the stream is reached when the
    tail is closed. *)
val create : ('a Tail.t -> unit) -> 'a t

(** [next t] returns a deferred that will become determined when the next part of the
    stream is determined.  This is [Cons (v, t')], where v is the next element of the
    stream and t' is the rest of the stream, or with Nil at the end of the stream. *)
type 'a next =
  | Nil
  | Cons of 'a * 'a t

val next : 'a t -> 'a next Deferred.t

(** [first_exn t] returns a deferred that becomes determined with the first element of
    [t]. *)
val first_exn : 'a t -> 'a Deferred.t

(** Streams can be converted to and from lists.  Although, conversion to a list returns a
    deferred, because the stream is determined asynchronously. *)

(** [of_list l] returns a stream with the elements of list l. *)
val of_list : 'a list -> 'a t

(** [to_list t] returns a deferred that will become determined with the list
    of elements in t, if the end of t is reached. *)
val to_list : 'a t -> 'a list Deferred.t

(** [of_fun f] returns a stream whose elements are determined by calling [f] forever. *)
val of_fun : (unit -> 'a Deferred.t) -> 'a t

(** [copy_to_tail t tail] reads elements from [t] and puts them in [tail], until
    the end of [t] is reached. *)
val copy_to_tail : 'a t -> 'a Tail.t -> unit Deferred.t

(** Sequence operations
    ----------------------------------------------------------------------
    There are the usual sequence operations:

    {v
      append, fold, iter, map, filter_map, take
    v}

    There are also deferred variants:

    {v
      iter', map', filter_map'
    v}

    These take anonymous functions that return deferreds generalizing the usual sequence
    operation and allowing the client to control the rate at which the sequence is
    processed. *)

(** [append t1 t2] returns a stream with all the values of t1, in order, and if t1 ends,
    these values are followed by all the values of t2. *)
val append : 'a t -> 'a t -> 'a t

(** [concat t] takes a stream of streams and produces a stream that is the concatenation
    of each stream in order (you see all of stream 1, then all of stream 2... etc.) *)
val concat : 'a t t -> 'a t

(** [available_now t] returns t prefix of t that is available now, along with the rest of
    the stream. *)
val available_now : 'a t -> 'a list * 'a t

(** [filter_deprecated s ~f] returns a stream with one element, v, for each v in s such
    with f v = true.

    Using [filter_deprecated] can easily lead to space leaks.  It is better to use
    [Async.Pipe] than [Async.Stream]. *)
val filter_deprecated : 'a t -> f:('a -> bool) -> 'a t

(** [filter_map_deprecated s ~f] returns a stream with one element, v', for each v in s
    such with f v = Some v'.

    Using [filter_map_deprecated] can easily lead to space leaks.  It is better to use
    [Async.Pipe] than [Async.Stream]. *)
val filter_map_deprecated : 'a t -> f:('a -> 'b option) -> 'b t

(** [fold' t ~init ~f] is like list fold, walking over the elements of the stream in
    order, as they become available.  [fold'] returns a deferred that will yield the final
    value of the accumulator, if the end of the stream is reached. *)
val fold' : 'a t -> init:'b -> f:('b -> 'a -> 'b Deferred.t) -> 'b Deferred.t

(** [fold t ~init ~f] is a variant of [fold'] in which [f] does not return a deferred. *)
val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b Deferred.t

(** [iter' t ~f] applies [f] to each element of the stream in turn, as they become
    available.  It continues onto the next element only after the deferred returned by [f]
    becomes determined. *)
val iter' : 'a t -> f:('a -> unit Deferred.t) -> unit Deferred.t

(** [closed t] returns a deferred that becomes determined when the end of [t] is
    reached.  *)
val closed : _ t -> unit Deferred.t

(** [iter t ~f] = [don't_wait_for (iter' t ~f:(fun a -> f a; return ()))] *)
val iter : 'a t -> f:('a -> unit) -> unit

(** [take_until t d] returns a stream [t'] that has the same elements as [t] up until [d]
    becomes determined. *)
val take_until : 'a t -> unit Deferred.t -> 'a t

(** [iter_durably' t ~f] is like [iter' t ~f], except if [f] raises an exception it
    continues with the next element of the stream *and* reraises the exception (to the
    monitor in scope when iter_durably was called).

    [iter_durably t ~f] is like [iter t ~f], except if [f] raises an exception it
    continues with the next element of the stream *and* reraises the exception (to the
    monitor in scope when iter_durably was called).

    [iter_durably_report_end t ~f] is equivalent to [iter_durably' t ~f:(fun x -> return
    (f x))] but it is more efficient *)
val iter_durably' : 'a t -> f:('a -> unit Deferred.t) -> unit Deferred.t

val iter_durably : 'a t -> f:('a -> unit) -> unit
val iter_durably_report_end : 'a t -> f:('a -> unit) -> unit Deferred.t

(** [length s] returns a deferred that is determined when the end of s is reached, taking
    the value of the number of elements in s *)
val length : 'a t -> int Deferred.t

(** [map' t f] creates a new stream that with one element, (f v), for each element v of
    t. *)
val map' : 'a t -> f:('a -> 'b Deferred.t) -> 'b t

(** [map t ~f] creates a new stream that with one element, (f v), for each element v of t.
    [map t f] = [map' t ~f:(fun a -> return (f a))]. *)
val map : 'a t -> f:('a -> 'b) -> 'b t

(** [first_n t n] returns a stream with the first n elements of t, if t has n or more
    elements, or it returns t. *)
val first_n : 'a t -> int -> 'a t

(** Stream generation
    ---------------------------------------------------------------------- *)

(** [unfold b f] returns a stream [a1; a2; ...; an] whose elements are
    determined by the equations:
    {v
      b0 = b
      Some (a1, b1) = f b0
      Some (a2, b2) = f b1
      ...
      None = f bn
    v} *)
val unfold : 'b -> f:('b -> ('a * 'b) option Deferred.t) -> 'a t

(** Miscellaneous operations
    ---------------------------------------------------------------------- *)

(** [split ~stop ~f t] returns a pair [(p, d)], where [p] is a prefix of [t] that ends
    for one of three reasons:
    {v
      1. [t] ends
      2. stop becomes determined
      3. f returns `Found
    v}
    The deferred [d] describes why the prefix ended, and returns the suffix of the
    stream in case (2) or (3). *)
val split
  :  ?stop:unit Deferred.t
  -> ?f:('a -> [ `Continue | `Found of 'b ])
  -> 'a t
  -> 'a t * [ `End_of_stream | `Stopped of 'a t | `Found of 'b * 'a t ] Deferred.t

(** [find ~f t] returns a deferred that becomes determined when [f x] is true for some
    element of [t], or if the end of the stream is reached *)
val find : 'a t -> f:('a -> bool) -> [ `End_of_stream | `Found of 'a * 'a t ] Deferred.t

(** [ungroup t] takes a stream of lists and unpacks the items from each list into a single
    stream *)
val ungroup : 'a list t -> 'a t

(** [interleave list] takes a stream of streams and returns a stream of their items
    interleaved as they become determined. The interleaved stream will be closed when the
    outer stream and all of the inner streams have been closed. *)
val interleave : 'a t t -> 'a t
