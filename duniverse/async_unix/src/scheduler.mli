(** Dispatches and monitors Async processes.

    The threading model is as follows.  Only one thread runs Async code at a time.  This
    is enforced by a single lock in Async's scheduler data structure.  There are any
    number of threads running code without holding the lock that get data from the outside
    world and want to affect the Async world.  They do this by calling
    [Thread_safe.run_in_async*], which acquires the lock, does a computation (e.g., fills
    an ivar), and then runs a "cycle" of Async computations. *)

open! Core
open! Import

type t = Raw_scheduler.t [@@deriving sexp_of]

include module type of struct
  include Async_kernel_scheduler
end

(** [t ()] returns the Async scheduler.  If the scheduler hasn't been created yet, this
    will create it and acquire the Async lock. *)
val t : unit -> t

(** Accessors *)

val max_num_open_file_descrs : unit -> int
val max_num_threads : unit -> int

(** [go ?raise_unhandled_exn ()] passes control to Async, at which point Async starts
    running handlers, one by one without interruption, until there are no more handlers to
    run.  When Async is out of handlers, it blocks until the outside world schedules more
    of them.  Because of this, Async programs do not exit until [shutdown] is called.

    [go ()] calls [handle_signal Sys.sigpipe], which causes the SIGPIPE signal to be
    ignored.  Low-level syscalls (e.g., write) still raise EPIPE.

    If any Async job raises an unhandled exception that is not handled by any monitor,
    Async execution ceases.  Then, by default, Async pretty prints the exception, and
    exits with status 1.  If you don't want this, pass [~raise_unhandled_exn:true], which
    will cause the unhandled exception to be raised to the caller of [go ()]. *)
val go : ?raise_unhandled_exn:bool (** default is [false] *) -> unit -> never_returns

(** [go_main] is like [go], except that you supply a [main] function that will be run to
    initialize the Async computation, and that [go_main] will fail if any Async has been
    used prior to [go_main] being called.  Moreover it allows you to configure more static
    options of the scheduler. *)
val go_main
  :  ?raise_unhandled_exn:bool (** default is [false] *)
  -> ?file_descr_watcher:Config.File_descr_watcher.t (** default is [Config] *)
  -> ?max_num_open_file_descrs:int (** default is [Config] *)
  -> ?max_num_threads:int (** default is [Config] *)
  -> main:(unit -> unit)
  -> unit
  -> never_returns

(** [report_long_cycle_times ?cutoff ()] sets up something that will print a warning to
    stderr whenever there is an Async cycle that is too long, as specified by [cutoff],
    whose default is 1s. *)
val report_long_cycle_times : ?cutoff:Time.Span.t -> unit -> unit

(** [is_running ()] returns true if the scheduler has been started. *)
val is_running : unit -> bool

(** [set_max_inter_cycle_timeout span] sets the maximum amount of time the scheduler will
    remain blocked (on epoll or select) between cycles. *)
val set_max_inter_cycle_timeout : Time.Span.t -> unit

(** [set_check_invariants do_check] sets whether Async should check invariants of its
    internal data structures.  [set_check_invariants true] can substantially slow down
    your program. *)
val set_check_invariants : bool -> unit

(** [set_detect_invalid_access_from_thread do_check] sets whether Async routines should
    check if they are being accessed from some thread other than the thread currently
    holding the Async lock, which is not allowed and can lead to very confusing
    behavior. *)
val set_detect_invalid_access_from_thread : bool -> unit

type 'b folder = { folder : 'a. 'b -> t -> (t, 'a) Field.t -> 'b }

(** [fold_fields ~init folder] folds [folder] over each field in the scheduler.  The
    fields themselves are not exposed -- [folder] must be a polymorphic function that
    can work on any field.  So, it's only useful for generic operations, e.g., getting
    the size of each field. *)
val fold_fields : init:'b -> 'b folder -> 'b

val is_ready_to_initialize : unit -> bool

(** If a process that has already created, but not started, the Async scheduler would like
    to fork, and would like the child to have a clean Async, i.e., not inherit any of the
    Async work that was done in the parent, it can call [reset_in_forked_process] at the
    start of execution in the child process.  After that, the child can do Async stuff and
    then start the Async scheduler. *)
val reset_in_forked_process : unit -> unit

(** [make_async_unusable ()] makes subsequent attempts to use the Async scheduler raise.
    One use case for [make_async_unusable] is if you fork from a process already running
    the Async scheduler, and want to run non-Async OCaml code in the child process, with
    the guarantee that the child process does not use Async. *)
val make_async_unusable : unit -> unit

(** [handle_thread_pool_stuck f] causes [f] to run whenever Async detects its thread pool
    is stuck (i.e., hasn't completed a job for over a second and has work waiting to
    start).  Async checks every second.  By default, if the thread pool has been stuck for
    less than 60s, Async will [eprintf] a message.  If more than 60s, Async will send an
    exception to the main monitor, which will abort the program unless there is a custom
    handler for the main monitor.

    Calling [handle_thread_pool_stuck] replaces whatever behavior was previously there. *)
val handle_thread_pool_stuck : (stuck_for:Time_ns.Span.t -> unit) -> unit

val default_handle_thread_pool_stuck : Thread_pool.t -> stuck_for:Time_ns.Span.t -> unit

(** [time_spent_waiting_for_io ()] returns the amount of time that the Async scheduler has
    spent in calls to [epoll_wait] (or [select]) since the start of the program. *)
val time_spent_waiting_for_io : unit -> Time_ns.Span.t

(** [set_min_inter_cycle_timeout] sets the minimum timeout that the scheduler will pass to
    the OS when it checks for I/O between cycles.  The minimum is zero by default.
    Setting it to a nonzero value is used to increase thread fairness between the
    scheduler and other threads.  A plausible setting is 10us.  This can also be set via
    the [ASYNC_CONFIG] environment variable. *)
val set_min_inter_cycle_timeout : Time_ns.Span.t -> unit
