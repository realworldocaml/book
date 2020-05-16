(** For shutting down an Async program. *)

open! Core
open! Import

(** [shutdown ?force status] initiates shutdown, which runs all the [at_shutdown]
    functions, waits for them to finish, and then exits with the supplied status.  The
    [at_shutdown] functions can block -- one can use [~force] to forcibly exit (with
    status 1) if the [at_shutdown] functions do not finish in a reasonable amount of time.

    By default, [force] is [after (sec 10.)].

    Repeated calls to [shutdown] with the same status will have no effect.  Any call to
    [shutdown] with nonzero status will cause that to be the status that is exited with.
    A call to [shutdown] with different nonzero status from the original call will
    raise. *)
val shutdown : ?force:unit Deferred.t -> int -> unit

(** [shutdown_on_unhandled_exn ()] arranges things so that whenever there is an
    asynchronous unhandled exception, an error message is printed to stderr and [shutdown
    1] is called.  This is useful when one wants to ensure that [at_shutdown] handlers run
    when there is an unhandled exception.  Calling [shutdown_on_unhandled_exn] ensures
    that [Scheduler.go] will not raise due to an unhandled exception, and instead that the
    program will exit once [at_shutdown] handlers finish. *)
val shutdown_on_unhandled_exn : unit -> unit

(** [exit ?force status] is [shutdown ?force status; Deferred.never ()].

    We do not have an exit function that returns a non-deferred:

    {[
      val exit : ?force:unit Deferred.t -> int -> _ ]}

    Such a function should not exist, for the same reason that we do not have:

    {[
      val block : 'a Deferred.t -> 'a ]}

    The semantics of such an exit function would allow one to block a running Async job,
    and to switch to another one (to run the [at_shutdown] handlers), without expressing
    that switch in the type system via a [Deferred.t].  That would eliminate all the nice
    reasoning guarantees that Async gives about concurrent jobs. *)
val exit : ?force:unit Deferred.t -> int -> _ Deferred.t

(** [default_force] returns the default [force] value used by [shutdown] and [exit]. *)
val default_force : unit -> unit -> unit Deferred.t

(** [set_default_force f] sets the default [force] value used by [shutdown] and [exit] to
    [f]. Initially, the default value is [fun () -> after (sec 10.)]. A subsequent call to
    [shutdown] or [exit] that doesn't supply [~force] will call [f] and will force
    shutdown when its result becomes determined.

    [set_default_force] has no effect if [shutdown] or [exit] has already been called, or
    if the next call to [shutdown] or [exit] supplies [~force].

    [set_default_force] is useful for applications that call [shutdown] indirectly via
    a library, yet want to modify its behavior.  *)
val set_default_force : (unit -> unit Deferred.t) -> unit

(** [shutting_down ()] reports whether we are currently shutting down, and if so, with
    what status. *)
val shutting_down : unit -> [ `No | `Yes of int ]

val is_shutting_down : unit -> bool

(** [at_shutdown f] causes [f ()] to be run when [shutdown] is called, and for [shutdown]
    to wait until the returned deferred finishes.  If [f] raises (synchronously or
    asynchronously), then the exception is printed to stderr and the program exits
    nonzero, irrespective of the status supplied to [shutdown].

    If [shutdown] has already been called, then calling [at_shutdown f] does nothing.

    The functions supplied to [at_shutdown] are run in parallel on shutdown.

    *)
val at_shutdown : (unit -> unit Deferred.t) -> unit

(** [don't_finish_before d] causes [shutdown] to wait until [d] becomes determined before
    finishing.  It is like [at_shutdown (fun _ -> d)], except it is more efficient, and
    will not take any space once [d] is determined.  There is a single [at_shutdown]
    shared among all deferreds supplied to [don't_finish_before].  [don't_finish_before]
    does not override the [force] argument passed to shutdown. *)
val don't_finish_before : unit Deferred.t -> unit
