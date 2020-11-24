(** A pointer to the end of an {!Async_stream} that can be used to extend the stream. *)

open! Import
module Deferred = Deferred1

type 'a t = 'a Types.Tail.t [@@deriving sexp_of]

(** [create ()] returns a new tail. *)
val create : unit -> _ t

(** [extend t v] extends the stream, and will raise an exception if [t] has been
    closed. *)
val extend : 'a t -> 'a -> unit

(** [close_exn t] closes [t].  Subsequent calls to [close_exn] or [extend]
    will raise an exception. *)
val close_exn : _ t -> unit

(** [close_if_open t] closes [t], if it's not already closed.  If [t] is already
    closed, then this is a no-op. *)
val close_if_open : _ t -> unit

(** [is_closed t] returns true iff the stream [t] is closed. *)
val is_closed : _ t -> bool

module Stream : sig
  type 'a t [@@deriving sexp_of]

  type 'a next =
    | Nil
    | Cons of 'a * 'a t

  val next : 'a t -> 'a next Deferred.t
end

(** [collect t] returns the stream starting at the current position of the tail, i.e. the
    stream consisting of all subsequent [extend]s. *)
val collect : 'a t -> 'a Stream.t
