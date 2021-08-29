(** Settings that globally affect the behavior of Async.

    These can be set by setting an environment variable, [ASYNC_CONFIG], to a sexp
    representation of the config.  Also, setting [ASYNC_CONFIG] to an invalid sexp
    (e.g. the empty string), will cause your program to print to stderr a usage message
    describing how to configure [ASYNC_CONFIG], and exit nonzero.  For example, the
    following shell command should print the usage message:

    {v
      ASYNC_CONFIG= foo.exe
    v} *)

open! Core_kernel
module Epoll_max_ready_events : Validated.S with type raw := int
module Max_inter_cycle_timeout : Validated.S with type raw := Time_ns.Span.t
module Min_inter_cycle_timeout : Validated.S with type raw := Time_ns.Span.t
module Max_num_threads : Validated.S with type raw := int
module Max_num_jobs_per_priority_per_cycle : Validated.S with type raw := int

module Max_num_open_file_descrs : sig
  include Validated.S with type raw := int
  include Equal.S with type t := t

  val default : t
end

module Dump_core_on_job_delay : sig
  module How_to_dump : sig
    type t =
      | Default
      | Call_abort
      | Call_gcore
    [@@deriving sexp]
  end

  type watch =
    { dump_if_delayed_by : Time_ns.Span.t
    ; how_to_dump : How_to_dump.t
    }
  [@@deriving sexp]

  type t =
    | Watch of watch
    | Do_not_watch
  [@@deriving sexp]
end

module Debug_tag : sig
  type t =
    | All
    | Clock
    | Fd
    | File_descr_watcher
    | Finalizers
    | Interruptor
    | Monitor
    | Monitor_send_exn
    | Parallel
    | Reader
    | Scheduler
    | Shutdown
    | Thread_pool
    | Thread_safe
    | Writer
  [@@deriving sexp_of]
end

module File_descr_watcher : sig
  type t =
    | Epoll_if_timerfd
    | Epoll
    | Select
  [@@deriving sexp_of]
end

type t =
  { abort_after_thread_pool_stuck_for : Time_ns.Span.t option
  ; check_invariants : bool option
  ; detect_invalid_access_from_thread : bool option
  ; dump_core_on_job_delay : Dump_core_on_job_delay.t option
  ; epoll_max_ready_events : Epoll_max_ready_events.t option
  ; file_descr_watcher : File_descr_watcher.t option
  ; max_inter_cycle_timeout : Max_inter_cycle_timeout.t option
  ; max_num_open_file_descrs : Max_num_open_file_descrs.t option
  ; max_num_threads : Max_num_threads.t option
  ; max_num_jobs_per_priority_per_cycle : Max_num_jobs_per_priority_per_cycle.t option
  ; min_inter_cycle_timeout : Min_inter_cycle_timeout.t option
  ; print_debug_messages_for : Debug_tag.t list option
  ; record_backtraces : bool option
  ; report_thread_pool_stuck_for : Time_ns.Span.t option
  ; thread_pool_cpu_affinity : Thread_pool_cpu_affinity.t option
  ; timing_wheel_config : Timing_wheel.Config.t option
  }
[@@deriving sexp_of]

val t : t
val environment_variable : string

module Print_debug_messages_for : sig
  val clock : bool
  val fd : bool
  val file_descr_watcher : bool
  val finalizers : bool
  val interruptor : bool
  val monitor : bool
  val monitor_send_exn : bool
  val parallel : bool
  val reader : bool
  val scheduler : bool
  val shutdown : bool
  val thread_pool : bool
  val thread_safe : bool
  val writer : bool
end

(** Documentation on these is in strings in config.ml, so it can be output in the
    help message. *)
val abort_after_thread_pool_stuck_for : Time_ns.Span.t

val check_invariants : bool
val detect_invalid_access_from_thread : bool
val dump_core_on_job_delay : Dump_core_on_job_delay.t
val thread_pool_cpu_affinity : Thread_pool_cpu_affinity.t
val epoll_max_ready_events : Epoll_max_ready_events.t
val file_descr_watcher : File_descr_watcher.t
val max_inter_cycle_timeout : Max_inter_cycle_timeout.t
val max_num_jobs_per_priority_per_cycle : Max_num_jobs_per_priority_per_cycle.t
val max_num_open_file_descrs : Max_num_open_file_descrs.t
val max_num_threads : Max_num_threads.t
val min_inter_cycle_timeout : Min_inter_cycle_timeout.t
val record_backtraces : bool
val report_thread_pool_stuck_for : Time_ns.Span.t
val timing_wheel_config : Timing_wheel.Config.t
val default_timing_wheel_config_for_word_size : Word_size.t -> Timing_wheel.Config.t

(** [!task_id] is used in debug messages.  It is is set in [Async_unix] to include
    the thread and pid. *)
val task_id : (unit -> Sexp.t) ref
