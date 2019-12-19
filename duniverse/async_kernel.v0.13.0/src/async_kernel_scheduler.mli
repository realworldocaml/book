(** The Async scheduler is responsible for running Async jobs.  It maintains the queue of
    jobs that need to run.  A "cycle" consists of running some (possibly all) jobs in the
    queue, along with some other bookkeeping, like advancing Async's clock to the current
    time. *)

open! Core_kernel
open! Import

type 'a with_options = ?monitor:Monitor.t -> ?priority:Priority.t -> 'a

val current_execution_context : unit -> Execution_context.t

(** [within_context context f] runs [f ()] right now with the specified execution
    context.  If [f] raises, then the exception is sent to the monitor of [context], and
    [Error ()] is returned. *)
val within_context : Execution_context.t -> (unit -> 'a) -> ('a, unit) Result.t

(** [within' f ~monitor ~priority] runs [f ()] right now, with the specified
    block group, monitor, and priority set as specified.  They will be reset to their
    original values when [f] returns.  If [f] raises, then the result of [within'] will
    never become determined, but the exception will end up in the specified monitor. *)
val within' : ((unit -> 'a Deferred.t) -> 'a Deferred.t) with_options

(** [within] is like [within'], but doesn't require the thunk to return a deferred. *)
val within : ((unit -> unit) -> unit) with_options

(** [within_v] is like [within], but allows a value to be returned by [f]. *)
val within_v : ((unit -> 'a) -> 'a option) with_options

(** [with_local key value ~f], when run in the current execution context, [e], runs [f]
    right now in a new execution context, [e'], that is identical to [e] except that
    [find_local key = value].  As usual, [e'] will be in effect in asynchronous
    computations started by [f].  When [with_local] returns, the execution context is
    restored to [e]. *)
val with_local : 'a Univ_map.Key.t -> 'a option -> f:(unit -> 'b) -> 'b

(** [find_local key] returns the value associated to [key] in the current execution
    context. *)
val find_local : 'a Univ_map.Key.t -> 'a option

(** Just like [within'], but instead of running the thunk right now, adds
    it to the Async queue to be run with other Async jobs. *)
val schedule' : ((unit -> 'a Deferred.t) -> 'a Deferred.t) with_options

(** Just like [schedule'], but doesn't require the thunk to return a deferred. *)
val schedule : ((unit -> unit) -> unit) with_options

(** [eneque_job execution_context.t f a] enqueues into the scheduler's job queue a job
    that will run [f a] in [execution_context]. *)
val enqueue_job : Execution_context.t -> ('a -> unit) -> 'a -> unit

(** [thread_safe_enqueue_job] is like [enqueue_job], except it is for use from a
    thread that doesn't hold the Async lock. *)
val thread_safe_enqueue_job : Execution_context.t -> ('a -> unit) -> 'a -> unit

(** [preserve_execution_context t f] saves the current execution context and returns a
    function [g] such that [g a] runs [f a] in the saved execution context.  [g a] becomes
    determined when [f a] becomes determined. *)
val preserve_execution_context : ('a -> unit) -> ('a -> unit) Staged.t

val preserve_execution_context' : ('a -> 'b Deferred.t) -> ('a -> 'b Deferred.t) Staged.t

(** [cycle_start ()] returns the result of [Time.now ()] called at the beginning of
    cycle. *)
val cycle_start : unit -> Time.t

val cycle_start_ns : unit -> Time_ns.t

(** [cycle_times ()] returns a stream that is extended with an element at the start of
    each Async cycle, with the amount of time that the previous cycle took, as determined
    by calls to [Time.now] at the beginning and end of the cycle. *)
val cycle_times : unit -> Time.Span.t Async_stream.t

val cycle_times_ns : unit -> Time_ns.Span.t Async_stream.t

(** [long_cycles ~at_least] returns a stream of cycles whose duration is at least
    [at_least].  [long_cycles] is more efficient than [cycle_times] because it only
    allocates a stream entry when there is a long cycle, rather than on every cycle. *)
val long_cycles : at_least:Time_ns.Span.t -> Time_ns.Span.t Async_stream.t

(** [cycle_count ()] returns the total number of Async cycles that have happened. *)
val cycle_count : unit -> int

(** [total_cycle_time ()] returns the total (wall) time spent executing jobs in Async
    cycles. *)
val total_cycle_time : unit -> Time_ns.Span.t

(** The [alarm_precision] of the timing-wheel used to implement Async's [Clock]. *)
val event_precision : unit -> Time.Span.t

val event_precision_ns : unit -> Time_ns.Span.t

(** [force_current_cycle_to_end ()] causes no more normal priority jobs to run in the
    current cycle, and for the end-of-cycle jobs (i.e., writes) to run, and then for the
    cycle to end. *)
val force_current_cycle_to_end : unit -> unit

(** [set_max_num_jobs_per_priority_per_cycle int] sets the maximum number of jobs that
    will be done at each priority within each Async cycle. The default is [500].
    [max_num_jobs_per_priority_per_cycle] retrieves the current value.  *)
val set_max_num_jobs_per_priority_per_cycle : int -> unit

val max_num_jobs_per_priority_per_cycle : unit -> int

(** [set_record_backtraces do_record] sets whether Async should keep in the execution
    context the history of stack backtraces (obtained via [Backtrace.get]) that led to the
    current job.  If an Async job has an unhandled exception, this backtrace history will
    be recorded in the exception.  In particular the history will appear in an unhandled
    exception that reaches the main monitor.  This can have a substantial performance
    impact, both in running time and space usage. *)
val set_record_backtraces : bool -> unit

(** [yield ()] returns a deferred that becomes determined after the current cycle
    completes.  This can be useful to improve fairness by [yield]ing within a computation
    to give other jobs a chance to run. *)
val yield : unit -> unit Deferred.t

(** [yield_until_no_jobs_remain ()] returns a deferred that becomes determined the next
    time Async's job queue is empty.  This is useful in tests when one needs to wait for
    the completion of all the jobs based on what's in the queue, when those jobs might
    create other jobs -- without depending on I/O or the passage of wall-clock time. *)
val yield_until_no_jobs_remain : unit -> unit Deferred.t

(** [yield_every ~n] returns a function that will act as [yield] every [n] calls and as
    [return ()] the rest of the time.  This is useful for improving fairness in
    circumstances where you don't have good control of the batch size, but can insert a
    deferred into every iteration.

    [yield_every] raises if [n <= 0].

    *)
val yield_every : n:int -> (unit -> unit Deferred.t) Staged.t

(** [num_jobs_run ()] returns the number of jobs that have been run since starting.  The
    returned value includes the currently running job. *)
val num_jobs_run : unit -> int

(** [num_pending_jobs] returns the number of jobs that are queued to run by the
    scheduler. *)
val num_pending_jobs : unit -> int

module Expert : sig
  val run_cycles_until_no_jobs_remain : unit -> unit
  val set_on_start_of_cycle : (unit -> unit) -> unit
  val set_on_end_of_cycle : (unit -> unit) -> unit
end

module Private = Scheduler
