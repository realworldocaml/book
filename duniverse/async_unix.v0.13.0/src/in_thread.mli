(** The [In_thread] module has functions for interaction between the Async world and other
    (kernel) threads.  The name is to remind us to think about threads and race
    conditions.

    All threads come from the one thread pool used for all Async-managed threads. *)

open! Core
open Async_kernel
module Priority : module type of Linux_ext.Priority with type t = Linux_ext.Priority.t

module Helper_thread : sig
  (** A Helper_thread is a thread that is dedicated to handling computations external to
      Async.  We need them because some libraries (e.g. Sqlite3) require that certain
      collections of computations run in the same thread. *)
  type t

  (** [create ?name ()] creates a new helper thread.  The [name] will be used as the
      thread name for any work that that is done by the thread that doesn't get its own
      name.

      [create] uses a thread from Async's thread pool, reserving that thread for exclusive
      use by the helper thread until the helper thread is no longer used (specifically,
      finalized and is finished with all its work), at which point the thread is made
      available for general use by the pool.

      [create] returns a deferred that becomes determined when a helper thread is
      available.  On the other hand, [create_now] checks if a helper thread is available
      now, and if so returns it, or else returns [Error]. *)
  val create : ?priority:Priority.t -> ?name:string -> unit -> t Deferred.t

  val create_now : ?priority:Priority.t -> ?name:string -> unit -> t Or_error.t
end

(** [pipe_of_squeue squeue] returns a pipe [p] and consumes the contents [squeue], placing
    them in [p].  It repeatedly grabs everything from [squeue], places it in [p], and
    then waits for pushback on [p]. *)
val pipe_of_squeue : 'a Squeue.t -> 'a Pipe.Reader.t

(** [When_finished] describes how [In_thread.run f] behaves when the helper thread
    finishes [f ()]. *)
module When_finished : sig
  type t =
    | Notify_the_scheduler
    (** The helper thread notifies the Async scheduler that the result is ready, so that
        the scheduler will wake up in a timely manner and run a cycle. *)
    | Take_the_async_lock
    (** The helper thread blocks until it can acquire the Async lock, at which point it
        runs a cycle. *)
    | Try_to_take_the_async_lock
    (** If the [thread_pool_cpu_affinity] is [Inherit], then the helper hread tries to
        take the Async lock and run a cycle.  If the [thread_pool_cpu_affinity] is
        [Cpuset] or the helper thread is unable to acquire the Async lock, then it
        behaves as in [Notify_the_scheduler]. *)
  [@@deriving enumerate, sexp_of]

  (** [default] defines the default value used for [In_thread.run]'s [?when_finished]
      argument.  Changes to [default] affect subsequent calls to [In_thread.run].
      Initially, [default = Try_to_take_the_async_lock], which typically leads to better
      latency by avoiding an extra context switch to pass the result to the Async
      scheduler thread.  However, there are applications (e.g. jenga) where
      [Notify_the_scheduler] leads to significantly higher throughput by greatly
      decreasing the total number of Async cycles. *)
  val default : t ref
end

(** [run ?priority ?thread ?name f] runs [f ()] in a separate thread outside Async and
    returns the result as a Deferred in the Async world.  If [f ()] raises an exception
    (asynchronously, since it is another thread) then that exception will be raised to the
    monitor that called [run].

    WARNING: Async code MUST NOT be used from within [f].  By Async code we mean
    pretty-much all functions of libraries making use of Async.  Only a few functions of
    the Async library can be called inside [In_thread.run].  These are explicitly marked
    as such, using the phrase "thread-safe".

    If [thread] is not supplied, then any thread from the thread pool could be used.  If
    you need to run routines in a specific thread (as is required by some libraries like
    Sqlite), you should create a helper thread and supply it to [run].

    If [priority] is supplied, the priority of the thread in the linux scheduler will be
    set to [priority] for the duration of [f ()], provided the thread is allowed to do so
    (see [man setpriority]).

    If you call [run] several times with the same helper thread, the [f ()] calls will run
    in sequence, in the order in which they are supplied to [run].  Each [f ()] will
    complete (return or raise) before another [f ()] starts.

    For example, if you do:

    {[
      let () =
        run ~thread f1;
        run ~thread f2;
        run ~thread f3; ]}

    Then the thread will run [f1 ()] to completion, then [f2 ()] to completion, then
    [f3 ()] to completion.

    If [name] is supplied, the name of the thread will be set to it for the duration of
    the execution of [f ()].

    [when_finished] describes how the helper thread behaves once [f ()] has completed.
    Its default value is the value of {!When_finished.default} when [run] is called. *)
val run
  :  ?priority:Priority.t
  -> ?thread:Helper_thread.t
  -> ?when_finished:When_finished.t
  -> ?name:string
  -> (unit -> 'a)
  -> 'a Deferred.t

(** [syscall f] runs f, which should be a single system call, and returns the result,
    handling the restarting of interrupted system calls.  To avoid race conditions, the
    [f] supplied to [syscall] should just make a system call.  That way, everything else
    is done holding the Async lock. *)
val syscall : name:string -> (unit -> 'a) -> ('a, exn) Result.t Deferred.t

val syscall_exn : name:string -> (unit -> 'a) -> 'a Deferred.t
