(** A [Bvar] is a synchronization point that allows one to [broadcast] a value to clients
    [wait]ing on the broadcast.  With a [Bvar], one can efficiently notify multiple
    clients of edge-triggered conditions, repeating as each edge trigger occurs.

    [Bvar] is like an {!module:Ivar}/{!module:Deferred}, except that it is
    always "empty" and can be repeatedly "filled" via {!broadcast}. If nobody
    is waiting on a [Bvar], then broadcasting to it is effectively a no-op, and
    the value that is broadcast is lost.

    Another way to view [Bvar] is as a restriction of [Condition] that supports only
    broadcast, not [signal]ing a single waiter.  Dropping [signal] simplifies the
    implementation significantly.

    An {!module:Mvar} can mimick a [unit Bvar.t] through appropriate use of its
    API, but is also capable of wider range of synchronization functionality
    than a [Bvar].

    The ['permissions] parameter is used for read/write permissions. See
    {!Perms} for more information. *)

open! Core
open! Import

type ('a, -'permissions) t = ('a, 'permissions) Types.Bvar.t [@@deriving sexp_of]

include Invariant.S2 with type ('a, 'permissions) t := ('a, 'permissions) t

val create : unit -> ('a, read_write) t

(** [wait t] becomes determined by the next call to [broadcast t a]. *)
val wait : ('a, [> read ]) t -> 'a Deferred0.t

(** [broadcast t a] causes {i all} of the non-determined deferreds returned from [wait t]
    to become determined with [a]. If no such deferreds exist, this operation is a
    no-op. *)
val broadcast : ('a, [> write ]) t -> 'a -> unit

(** [has_any_waiters t] returns [true] iff there has been a call to [wait t] since the
    most recent call to [broadcast t]. *)
val has_any_waiters : ('a, _) t -> bool
