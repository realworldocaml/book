(** A [Bvar] is a synchronization point that allows one to [broadcast] a value to clients
    [wait]ing on the broadcast.  With a [Bvar], one can efficiently notify multiple
    clients of edge-triggered conditions, repeating as each edge trigger occurs.

    [Bvar] is like an ivar/deferred, except that it is always "empty" and can be
    repeatedly "filled" (via [broadcast]).

    Another way to view [Bvar] is as a restriction of [Condition] that supports only
    broadcast, not [signal]ing a single waiter.  Dropping [signal] simplifies the
    implementation significantly.

    The ['permissions] parameter is used for read/write permissions.  Also see [Perms]. *)

open! Core_kernel
open! Import

type ('a, -'permissions) t = ('a, 'permissions) Types.Bvar.t [@@deriving sexp_of]

include Invariant.S2 with type ('a, 'permissions) t := ('a, 'permissions) t

val create : unit -> ('a, read_write) t

(** [wait t] becomes determined by the next call to [broadcast t a]. *)
val wait : ('a, [> read]) t -> 'a Deferred0.t

val broadcast : ('a, [> write]) t -> 'a -> unit

(** [has_any_waiters t] returns [true] iff there has been a call to [wait t] since the
    most recent call to [broadcast t]. *)
val has_any_waiters : ('a, _) t -> bool
