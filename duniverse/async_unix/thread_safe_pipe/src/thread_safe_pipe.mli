(** A thread-safe pipe is a thread-safe interface to the write end of a normal
    [Async.Pipe].  {b All operations except for [create] must be called from threads
    outside Async}, while [create] can be called from inside or outside Async.

    For [Pipe] functions that return a [unit Deferred.t], the analog in [Thread_safe_pipe]
    blocks.

    For documentation of [wakeup_scheduler], see the {!Thread_safe} module. *)

open! Core
open! Async_kernel
open! Import

(** The writer end of the pipe. *)
type 'a t [@@deriving sexp_of]

(** [create ()] returns a reader end, which must be used inside Async, and a writer end,
    which must be used outside Async.  [create] can be called inside or outside Async. *)
val create : unit -> 'a Pipe.Reader.t * 'a t

(** All the following functions must be called outside Async.  They behave as their
    counterpart in the {!Pipe} module. *)

(** [pushback writer] blocks the current thread until the pipe is empty or closed. *)
val pushback : _ t -> unit

module Written_or_closed : sig
  type t =
    | Written
    | Closed
end

(** Functions that write elements to the pipe take an [If_closed.t] argument to specify
    how to deal with the possibility that the pipe is closed.

    The alternatives are to [Raise] on a closed pipe, or [Return] a variant indicating
    whether the pipe is closed.  This allows lightweight syntax for calls that want to
    raise if the pipe is closed:

    {[
      write t a ~if_closed:Raise ]}

    It also allows lightweight syntax for calls that want to match on whether the pipe was
    closed:

    {[
      match write t a ~if_closed:Return with
      | Closed  -> ...
      | Written -> ... ]}

    Returning a variant is essential when one wants to distinguish a closed pipe from
    other errors.  Also, since pipe-writing functions acquire the Async lock, it would be
    incorrect (due to races) to check [is_closed] prior to the lock acquisition. *)
module If_closed : sig
  type 'a t =
    | Raise : unit t
    | Return : Written_or_closed.t t
end

(** [transfer_in_without_pushback'] and [write_without_pushback] transfer the element(s)
    into the pipe and return immediately. *)
val transfer_in_without_pushback
  :  ?wakeup_scheduler:bool (** default is [true] *)
  -> 'a t
  -> from:'a Queue.t
  -> if_closed:'b If_closed.t
  -> 'b

val write_without_pushback
  :  ?wakeup_scheduler:bool (** default is [true] *)
  -> 'a t
  -> 'a
  -> if_closed:'b If_closed.t
  -> 'b

(** [transfer_in] and [write] transfer the element(s) into the pipe and block the current
    thread until the pipe is empty or closed (like {!pushback}). *)
val transfer_in : 'a t -> from:'a Queue.t -> if_closed:'b If_closed.t -> 'b

val write : 'a t -> 'a -> if_closed:'b If_closed.t -> 'b
val close : _ t -> unit
val is_closed : _ t -> bool

(** [closed writer] blocks the current thread until the pipe is closed. *)
val closed : _ t -> unit
