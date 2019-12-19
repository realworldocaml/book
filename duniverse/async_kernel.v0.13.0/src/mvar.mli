(** An [Mvar] is a mutable location that is either empty or contains a value.  One can
    [put] or [set] the value, and wait on [value_available] for the location to be filled
    in either way.

    Having an [Mvar.Read_write.t] gives the capability to mutate the mvar.

    The key difference between an [Mvar] and an {{!Async_kernel.Ivar}[Ivar]} is that an
    [Mvar] may be filled multiple times.

    This implementation of [Mvar] also allows one to replace the value without any
    guarantee that the reading side has seen it.  This is useful in situations where
    last-value semantics are desired (e.g., you want to signal whenever a config file is
    updated, but only care about the most recent contents).

    An [Mvar] can also be used as a baton-passing mechanism between a producer and
    consumer.  For instance, a producer reading from a socket and producing a set of
    deserialized messages can [put] the batch from a single read into an [Mvar] and can
    wait for [taken] to return as a pushback mechanism.  The consumer meanwhile waits on
    [value_available].  This way the natural batch size is passed between the two
    sub-systems with minimal overhead. *)

open! Core_kernel
open! Import

type ('a, -'phantom) t [@@deriving sexp_of]

module Read_write : sig
  type nonrec 'a t = ('a, read_write) t [@@deriving sexp_of]

  include Invariant.S1 with type 'a t := 'a t
end

module Read_only : sig
  type nonrec 'a t = ('a, read) t [@@deriving sexp_of]

  include Invariant.S1 with type 'a t := 'a t
end

val create : unit -> 'a Read_write.t
val is_empty : (_, _) t -> bool

(** [put t a] waits until [is_empty t], and then does [set t a].  If there are multiple
    concurrent [put]s, there is no fairness guarantee (i.e., [put]s may happen out of
    order or may be starved). *)
val put : ('a, [> write ]) t -> 'a -> unit Deferred.t

(** [set t a] sets the value in [t] to [a], even if [not (is_empty t)].  This is useful if
    you want takers to have last-value semantics. *)
val set : ('a, [> write ]) t -> 'a -> unit

(** [update t ~f] applies [f] to the value in [t] and [set]s [t] to the result.  This is
    useful if you want takers to have accumulated-value semantics. *)
val update : ('a, read_write) t -> f:('a option -> 'a) -> unit

(** [update_exn] is like [update], except it raises if [is_empty t]. *)
val update_exn : ('a, read_write) t -> f:('a -> 'a) -> unit

val read_only : ('a, [> read ]) t -> ('a, read) t
val write_only : ('a, [> write ]) t -> ('a, write) t

(** [value_available t] returns a deferred [d] that becomes determined when a value is in
    [t].  [d] does not include the value in [t] because that value may change after [d]
    becomes determined and before a deferred bind on [d] gets to run.

    Repeated calls to [value_available t] will always return the same deferred until
    the [t] is filled. *)
val value_available : (_, [> read ]) t -> unit Deferred.t

(** [take t] returns a deferred that, when [t] is filled, becomes determined with the
    value of [t] and and clears [t].  If there are multiple concurrent calls to [take]
    then only one of them will be fulfilled and the others will continue waiting on future
    values.  There is no ordering guarantee for which [take] call will be filled first. *)
val take : ('a, [> read ]) t -> 'a Deferred.t

(** [take_now] is an immediate form of [take]. *)
val take_now : ('a, [> read ]) t -> 'a option

val take_now_exn : ('a, [> read ]) t -> 'a

(** [taken t] returns a deferred that is filled the next time [take] clears [t]. *)
val taken : (_, [> write ]) t -> unit Deferred.t

(** [peek t] returns the value in [t] without clearing [t], or returns [None] is [is_empty
    t]. *)
val peek : ('a, [> read ]) t -> 'a option

(** [peek_exn t] is like [peek], except it raises if [is_empty t]. *)
val peek_exn : ('a, [> read ]) t -> 'a

(** [pipe_when_ready t] returns a pipe, then repeatedly takes a value from [t] and writes
    it to the pipe.  After each write, [pipe_when_ready] waits for the pipe to be ready to
    accept another value before taking the next value.  Once the pipe is closed,
    [pipe_when_ready] will no longer take values from [t].

    Notice that this implementation effectively creates an extra buffer of size 1, so when
    you read from the pipe you can read a stale value (even though a fresh one should come
    immediately afterwards), and a value will be taken from the MVar even if it's never
    read from the pipe.

    There is no protection against creating multiple pipes or otherwise multiple things
    trying to [take] concurrently.  If that happens, it's not specified which of the pipes
    will get the value. *)
val pipe_when_ready : ('a, [> read ]) t -> 'a Pipe.Reader.t
