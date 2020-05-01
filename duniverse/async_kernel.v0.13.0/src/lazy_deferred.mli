(** A delayed computation that can produce a deferred.

    Nothing happens with a lazy deferred unless one [force]s it.  Forcing a lazy deferred
    starts the computation, which will eventually cause the deferred to become determined.
    As usual with laziness, multiply forcing a lazy deferred is no different than forcing
    it a single time.

    Exceptions (both synchronous and asynchronous) raised by a delayed computation are
    returned by [force] ([wait], [peek], etc.), or will be raised to the monitor in effect
    when [force_exn] ([wait_exn], [peek_exn], etc.) was called.

    The type is not exposed nor defined as ['a Deferred.t Lazy.t] or ['a Or_error.t
    Deferred.t Lazy.t], because there is a difference in power with these types.  Any
    value of type ['a Deferred.t Lazy.t] would mishandle asynchronous exceptions in the
    computation of ['a].  For instance, the following code blocks forever regardless of
    how [v] is defined:

    {[
      let v : Nothing.t Deferred.t Lazy.t = lazy (return "" >>| failwith) in
      let%bind _ = try_with (fun () -> force v) in
      let%bind _ = try_with (fun () -> force v) in
    ]}

    There is no [val of_lazy : 'a Deferred.t Lazy.t -> 'a t] because of the difference
    in power.

    See also [Deferred.Memo.unit], if you only are interested in [create] and [force]. *)

open! Core_kernel

type 'a t

(** [create f] creates a new lazy deferred that will call [f] when it is forced. *)
val create : (unit -> 'a Deferred.t) -> 'a t

(** [force t] forces evaluation of [t] and returns a deferred that becomes determined
    when the deferred computation becomes determined or raises. *)
val force : 'a t -> 'a Or_error.t Deferred.t

val force_exn : 'a t -> 'a Deferred.t

(** [wait t] and [wait_exn t] waits for [t] to be forced.  If no one ever calls
    [force t], they will wait forever. *)
val wait : 'a t -> 'a Or_error.t Deferred.t

val wait_exn : 'a t -> 'a Deferred.t

(** [bind t f] in the lazy-deferred monad creates a computation that, when forced, will
    force [t], apply [f] to the result, and then force the result of that. *)
include
  Monad with type 'a t := 'a t

(** [bind'] differs from [bind] in that the supplied function produces an ['a Deferred.t]
    rather than an ['a t]. *)
val bind' : 'a t -> ('a -> 'b Deferred.t) -> 'b t

(** Read-only operations. *)

(** [peek t = Deferred.peek (wait t)] *)
val peek : 'a t -> 'a Or_error.t option

val peek_exn : 'a t -> 'a option
val is_determined : _ t -> bool
val is_forced : _ t -> bool
