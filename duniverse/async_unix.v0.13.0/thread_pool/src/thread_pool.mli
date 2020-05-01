(** A thread pool is a set of OCaml threads used to do work, where each piece of work is
    simply a thunk.  One creates a thread pool, and then uses [add_work] to submit work to
    it.  Work is done first-come-first-served by available threads in the pool.  Any of
    the available threads in the pool could be used to do work submitted to the pool
    (except helper threads, see below).

    A thread pool starts with no threads.  As work is added, the thread pool creates new
    threads to do the work, up to the maximum number of allowed threads,
    [max_num_threads], supplied to [create].  Thread-pool threads never die.  They just
    get created up until [max_num_threads] is reached and then live forever, doing work.
    Each thread in the pool is in a loop, waiting for a piece of work, running the thunk,
    and then repeating.  It may be that all the threads in the pool are not doing
    anything, but in this case, the threads still exist, and are simply blocked waiting
    for work.

    Sometimes one wants work to run in a dedicated thread, e.g. some C libraries require
    this.  To do this, use [Helper_thread], see below.

    All of the functions exposed by this module are thread safe; they synchronize using a
    mutex on the thread pool.

    One must not call thread-pool functions from a GC finalizer, since a finalizer could
    run within a thread running a thread-pool function, which already holds the lock, and
    would therefore deadlock or error when attempting to re-acquire it.  This is
    accomplished elsewhere by using Async finalizers, which are run from ordinary Async
    jobs, and thus do not hold the thread-pool lock.

    One can control the priority and affinity of threads in the pool (priority in the
    sense of [Linux_ext.setpriority]).
    Work added to the pool can optionally be given a priority, and the
    pool will set the priority of the thread that runs it for the duration of the work.
    Helper threads can also be given a priority, which will be used for
    all work run by the helper thread, unless the work has an overriding priority.
    The thread pool has a "default" priority that will be used for all work
    and helper threads that have no specified priority.
    The default priority is the priority in effect when [create] is called.

    Affinity, on the other hand, can only be specified when you create a pool.
    The default affinity is the affinity in effect when a new thread happens to be created
    (e.g. when you call [add_work]).

    Behavior is unspecified if work calls [setpriority] or [setaffinity] directly. *)

open! Core
open! Import
module Cpu_affinity = Thread_pool_cpu_affinity
module Priority : module type of Linux_ext.Priority with type t = Linux_ext.Priority.t

type t [@@deriving sexp_of]

include Invariant.S with type t := t

(** [create ?cpuset ~max_num_threads] returns a new thread pool.  It is an
    error if [max_num_threads < 1].

    If [cpuset] is specified, then every thread will be affinitized to those
    CPU cores upon creation.

    If [cpuset] is not specified, then every thread will inherit the
    affinitization of the thread (typically the main thread) that created it. *)
val create
  :  ?cpu_affinity:Cpu_affinity.t (** default is [Inherit] *)
  -> max_num_threads:int
  -> unit
  -> t Or_error.t

(** [cpu_affinity t] returns the CPU affinity that [t] was created with.  All
    threads created by [t] will be created with this affinity. *)
val cpu_affinity : t -> Cpu_affinity.t

(** [finished_with t] makes it an error to subsequently call [add_work* t] or
    [create_helper_thread t].  And, once all current work in [t] is finished, destroys all
    the threads in [t].  It is OK to call [finished_with] multiple times on the same [t];
    subsequent calls will have no effect. *)
val finished_with : t -> unit

(** [block_until_finished t] blocks the current thread until thread pool [t] has finished.
    One must previously have called [finished_with] to cause [t] to start finishing. *)
val block_until_finished : t -> unit

(** [max_num_threads t] returns the maximum number of threads that [t] is allowed to
    create. *)
val max_num_threads : t -> int

(** [num_threads t] returns the number of threads that the pool [t] has created. *)
val num_threads : t -> int

(** [unfinished_work t] returns the number of jobs that have been submitted to [t]
    but haven't yet finished. *)
val unfinished_work : t -> int

(** [default_priority t] returns the priority that will be used for work performed by
    [t], unless that work is added with an overriding priority. *)
val default_priority : t -> Priority.t

(** [add_work ?priority ?name t f] enqueues [f] to be done by some thread in the pool.

    Exceptions raised by [f] are silently ignored.

    While the work is run, the name of the thread running the work will be set (via
    [Linux_ext.pr_set_name]) to [name] and the priority of the thread will be set
    to [priority].

    It is an error to call [add_work t] after [finished_with t]. *)
val add_work
  :  ?priority:Priority.t (** default is [default_priority t] *)
  -> ?name:string (** default is ["thread-pool thread"] *)
  -> t
  -> (unit -> unit)
  -> unit Or_error.t

val num_work_completed : t -> int

(** [has_unstarted_work t] returns [true] if [t] has work that it hasn't been assigned
    to start running in a thread. *)
val has_unstarted_work : t -> bool

(** A helper thread is a thread with its own dedicated work queue.  Work added for the
    helper thread is guaranteed to be run by that thread.  The helper thread only runs
    work explicitly supplied to it.  Helper threads count towards a thread pool's
    [max_num_threads]. *)
module Helper_thread : sig
  type t [@@deriving sexp_of]

  (** [default_name t] returns the name that will be used for work performed by [t],
      unless that work is added with an overriding name *)
  val default_name : t -> string

  (** [default_priority t] returns the priority that will be used for work performed by
      [t], unless that work is added with an overriding priority. *)
  val default_priority : t -> Priority.t
end

(** [create_helper_thread ?priority ?name t] takes an available thread from the thread
    pool and makes it a helper thread, raising if no threads are available or if
    [finished_with t] was previously called.  The new helper thread runs work with [name]
    and [priority], except for work that is added with an overriding priority or name.
    The thread remains a helper thread until [finished_with_helper_thread] is called, if
    ever. *)
val create_helper_thread
  :  ?priority:Priority.t (** default is [default_priority t] *)
  -> ?name:string (** default is ["helper thread"] *)
  -> t
  -> Helper_thread.t Or_error.t

(** [become_helper_thread ?priority ?name t] should be run from within work supplied to
    [add_work].  When [become_helper_thread] runs, it transitions the current thread into
    a helper thread.

    Other than that, [become_helper_thread] is like [create_helper_thread], except it
    cannot fail because no threads are available. *)
val become_helper_thread
  :  ?priority:Priority.t (** default is [default_priority t] *)
  -> ?name:string (** default is ["helper thread"] *)
  -> t
  -> Helper_thread.t Or_error.t

(** [add_work_for_helper_thread ?priority ?name t helper_thread f] enqueues [f] on
    [helper_thread]'s work queue.

    Exceptions raised by [f] are silently ignored.

    It is an error to call [add_work_for_helper_thread t] after
    [finished_with_helper_thread t].

    When the helper thread runs [f], it will be at the helper thread's name and priority,
    unless overriden by [name] or [priority]. *)
val add_work_for_helper_thread
  :  ?priority:Priority.t (** default is [Helper_thread.default_priority helper_thread] *)
  -> ?name:string (** default is [Helper_thread.name helper_thread] *)
  -> t
  -> Helper_thread.t
  -> (unit -> unit)
  -> unit Or_error.t

(** [finished_with_helper_thread t helper_thread] informs thread pool [t] that no future
    work will be added for [helper_thread], and makes it an error to in the future add
    work for [helper_thread].  Furthermore, once [helper_thread] finishes with its last
    piece of work, it will revert to a general thread-pool thread.  It is OK to call
    [finished_with_helper_thread] multiple times on the same [helper_thread]; subsequent
    calls will have no effect. *)
val finished_with_helper_thread : t -> Helper_thread.t -> unit

val last_thread_creation_failure : t -> Sexp.t option
val thread_creation_failure_lockout : t -> Time_ns.Span.t
val debug : bool ref

(**/**)

module Private : sig
  val check_invariant : bool ref
  val default_thread_name : string
  val is_finished : t -> bool
  val is_in_use : t -> bool
  val set_last_thread_creation_failure : t -> Time_ns.t -> unit
  val set_thread_creation_failure_lockout : t -> Time_ns.Span.t -> unit
end
