(** The [Thread_safe] module has functions that are safe to call from threads outside
    Async, such as the ones spawned by [In_thread.run].

    This is in contrast with the rest of [Async] library which is generally not
    thread-safe.

    All the [Thread_safe.block*] and [Thread_safe.run*] functions wake up the Async
    scheduler to ensure that it continues in a timely manner with whatever jobs got
    started.  Some functions take an optional [?wakeup_scheduler:bool] argument, which
    defaults to [true].  One can cause the scheduler to not be woken up by supplying
    [~wakeup_scheduler:false], which can reduce CPU use, but increase latency, because the
    scheduler may not wake up for a while to process jobs. *)

open! Core
open Async_kernel

(** [am_holding_async_lock ()] returns true if the currently running thread is holding the
    Async lock. *)
val am_holding_async_lock : unit -> bool

(** [deferred ()] returns [(d, fill)] where [d] is a deferred that will become determined
    with value [v] once [fill v] is called.

    It is ok to call [deferred] from inside or outside Async.  [fill] must be called from
    outside Async.
*)
val deferred : unit -> 'a Deferred.t * ('a -> unit)

(** [run_in_async_with_optional_cycle f] acquires the Async lock and runs [f ()] while
    holding the lock.  Depending on the result of [f], it may also run a cycle.
*)
val run_in_async_with_optional_cycle
  :  ?wakeup_scheduler:bool (** default is [true] *)
  -> (unit -> [ `Run_a_cycle | `Do_not_run_a_cycle ] * 'a)
  -> ('a, exn) Result.t

(** [run_in_async f] acquires the Async lock and runs [f ()] while holding the lock. It
    returns the result of [f ()] to the outside world.  The scheduler is woken up to
    ensure the code that depends on [f ()] is run soon enough.

    [run_in_async] doesn't run a cycle.

    [run_in_async] does not automatically start the Async scheduler.  You still need to
    call [Scheduler.go] elsewhere in your program.
*)
val run_in_async
  :  ?wakeup_scheduler:bool (** default is [true] *)
  -> (unit -> 'a)
  -> ('a, exn) Result.t

val run_in_async_exn
  :  ?wakeup_scheduler:bool (** default is [true] *)
  -> (unit -> 'a)
  -> 'a

(** [block_on_async f] runs [f ()] in the Async world and blocks until the result becomes
    determined.  This function can be called from the main thread (before Async is
    started) or from a thread outside Async.

    [block_on_async] will run a cycle if the deferred isn't determined, in the hope that
    running the cycle will cause the deferred to become determined.

    [block_on_async] will automatically start the scheduler if it isn't already
    running.
*)
val block_on_async : (unit -> 'a Deferred.t) -> ('a, exn) Result.t

val block_on_async_exn : (unit -> 'a Deferred.t) -> 'a

(** [run_in_async_wait f] is like [block_on_async f], except that it must be called from a
    thread outside Async.  Upon returning from [run_in_async_wait], it is guaranteed that
    the caller does not have the Async lock.
*)
val run_in_async_wait : (unit -> 'a Deferred.t) -> ('a, exn) Result.t

val run_in_async_wait_exn : (unit -> 'a Deferred.t) -> 'a

(** [reset_scheduler] stops the scheduler thread and any associated threads, and resets
    Async's global state to its initial state.  This is useful if you need to first use
    Async to compute a value and then to daemonize (in which case you should [daemonize]
    with [~allow_threads_to_have_been_created:true]).

    [reset_scheduler] can be called from the main thread (before Async is started) or from
    a thread outside Async.  [reset_scheduler] is known to be imperfect, and to have races
    in which there are still threads running after it returns. *)
val reset_scheduler : unit -> unit
