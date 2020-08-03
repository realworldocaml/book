open Core_kernel
open Import
include Scheduler0
module Synchronous_time_source = Synchronous_time_source0
module Event = Synchronous_time_source.Event
module Alarm = Timing_wheel.Alarm
module Job_or_event = Synchronous_time_source.T1.Job_or_event

let debug = Debug.scheduler

module Ivar = struct
  open Types.Ivar

  let create_with_cell cell = { cell }
  let create () = create_with_cell Empty

  let create_full (type a) (a : a) =
    (* We allocate an immutable ivar and then cast it to a mutable ivar.  The immutability
       allows OCaml to statically allocate the ivar if [a] is constant.  This cast is safe
       because a full ivar is never mutated.  We also believe that we will not trigger
       flambda to spuriously repor warning 59, mutation of known immutable data.  All
       mutations of an ivar cell, i.e. [foo.cell <- ...], are directly preceded by a
       [match foo.cell] that prevents the [Full] case from reaching the modification.  So
       flambda should always eliminate the [foo.cell <- ...] of a constant [Full] ivar,
       and not warn. *)
    (Obj.magic : a Immutable.t -> a t) { cell = Full a }
  ;;
end

module Bvar = struct
  open Types.Bvar

  let create () = of_repr { has_any_waiters = false; ivar = Ivar.create () }
end

module Very_low_priority_worker = struct
  module Exec_result = struct
    type t = Types.Very_low_priority_worker.Exec_result.t =
      | Finished
      | Not_finished
    [@@deriving sexp_of]
  end

  type t = Types.Very_low_priority_worker.t =
    { execution_context : Execution_context.t
    ; exec : unit -> Exec_result.t
    }
  [@@deriving fields, sexp_of]

  let invariant t =
    Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter ~execution_context:(check Execution_context.invariant) ~exec:ignore)
  ;;
end

type t = Scheduler0.t =
  { (* [check_access] optionally holds a function to run to check whether access to [t] is
       currently allowed.  It is used to detect invalid access to the scheduler from a
       thread. *)
    mutable check_access : (unit -> unit) option
  ; mutable job_pool : Job_pool.t
  ; normal_priority_jobs : Job_queue.t
  ; low_priority_jobs : Job_queue.t
  ; very_low_priority_workers : Very_low_priority_worker.t Deque.t
  ; mutable main_execution_context : Execution_context.t
  ; mutable current_execution_context : Execution_context.t
  (* The scheduler calls [got_uncaught_exn] when an exception bubbles to the top of the
     monitor tree without being handled.  This function guarantees to never run another
     job after this by calling [clear] and because [enqueue_job] will never add another
     job. *)
  ; mutable uncaught_exn : (Exn.t * Sexp.t) option
  ; mutable cycle_count : int
  ; mutable cycle_start : Time_ns.t
  ; mutable in_cycle : bool
  ; mutable run_every_cycle_start : (unit -> unit) list
  ; mutable run_every_cycle_end : (unit -> unit) list
  ; mutable last_cycle_time : Time_ns.Span.t
  ; mutable last_cycle_num_jobs : int
  ; mutable total_cycle_time : Time_ns.Span.t
  ; mutable time_source : read_write Synchronous_time_source.T1.t
  (* [external_jobs] is a queue of actions sent from outside of async.  This is for the
     case where we want to schedule a job or fill an ivar from a context where it is not
     safe to run async code, because the async lock isn't held.  For instance: - in an
     OCaml finalizer, as they can run at any time in any thread.

     The way to do it is to queue a thunk in [external_jobs] and call
     [thread_safe_external_job_hook], which is responsible for notifying the scheduler
     that new actions are available.

     When using Async on unix, [thread_safe_external_job_hook] is set in [Async_unix]
     to call [Interruptor.thread_safe_interrupt], which will wake up the
     [Async_unix] scheduler and run a cycle.

     Note that this hook might be used in other context (js_of_ocaml, mirage).

     When running a cycle, we pull external actions at every job and perform them
     immediately. *)
  ; external_jobs : External_job.t Thread_safe_queue.t
  ; mutable thread_safe_external_job_hook : unit -> unit
  (* [job_queued_hook] and [event_added_hook] aim to be used by js_of_ocaml. *)
  (* We use [_ option] here because those hooks will not be set in the common case
     and we want to avoid extra function calls. *)
  ; mutable job_queued_hook : (Priority.t -> unit) option
  ; mutable event_added_hook : (Time_ns.t -> unit) option
  ; mutable yield : ((unit, read_write) Types.Bvar.t[@sexp.opaque])
  ; mutable yield_until_no_jobs_remain :
      ((unit, read_write) Types.Bvar.t[@sexp.opaque] (* configuration*))
  ; mutable check_invariants : bool
  ; mutable max_num_jobs_per_priority_per_cycle : Max_num_jobs_per_priority_per_cycle.t
  ; mutable record_backtraces : bool
  ; mutable on_start_of_cycle : unit -> unit
  ; mutable on_end_of_cycle : unit -> unit
  }
[@@deriving fields, sexp_of]

let uncaught_exn_unwrapped = uncaught_exn

let uncaught_exn t =
  match t.uncaught_exn with
  | None -> None
  | Some (exn, sexp) ->
    Some (Error.create "unhandled exception" (exn, sexp) [%sexp_of: Exn.t * Sexp.t])
;;

let num_pending_jobs t =
  Job_queue.length t.normal_priority_jobs + Job_queue.length t.low_priority_jobs
;;

let num_jobs_run t =
  Job_queue.num_jobs_run t.normal_priority_jobs
  + Job_queue.num_jobs_run t.low_priority_jobs
;;

let last_cycle_num_jobs t = t.last_cycle_num_jobs

let invariant t : unit =
  try
    let check f field = f (Field.get field t) in
    Fields.iter
      ~check_access:ignore
      ~job_pool:(check Job_pool.invariant)
      ~normal_priority_jobs:(check Job_queue.invariant)
      ~low_priority_jobs:(check Job_queue.invariant)
      ~very_low_priority_workers:
        (check (fun q -> Deque.iter q ~f:Very_low_priority_worker.invariant))
      ~main_execution_context:(check Execution_context.invariant)
      ~current_execution_context:(check Execution_context.invariant)
      ~uncaught_exn:
        (check (fun uncaught_exn ->
           if is_some uncaught_exn then assert (num_pending_jobs t = 0)))
      ~cycle_count:(check (fun cycle_count -> assert (cycle_count >= 0)))
      ~cycle_start:ignore
      ~in_cycle:ignore
      ~run_every_cycle_start:ignore
      ~run_every_cycle_end:ignore
      ~last_cycle_time:ignore
      ~total_cycle_time:ignore
      ~last_cycle_num_jobs:
        (check (fun last_cycle_num_jobs -> assert (last_cycle_num_jobs >= 0)))
      ~time_source:
        (check
           (Synchronous_time_source.Read_write.invariant_with_jobs ~job:(fun job ->
              assert (Pool.pointer_is_valid t.job_pool job))))
      ~external_jobs:ignore
      ~thread_safe_external_job_hook:ignore
      ~job_queued_hook:ignore
      ~event_added_hook:ignore
      ~yield:ignore
      ~yield_until_no_jobs_remain:ignore
      ~check_invariants:ignore
      ~max_num_jobs_per_priority_per_cycle:ignore
      ~record_backtraces:ignore
      ~on_start_of_cycle:ignore
      ~on_end_of_cycle:ignore
  with
  | exn -> raise_s [%message "Scheduler.invariant failed" (exn : exn) (t : t)]
;;

let free_job t job = Pool.free t.job_pool job

let enqueue t (execution_context : Execution_context.t) f a =
  (* If there's been an uncaught exn, we don't add the job, since we don't want any jobs
     to run once there's been an uncaught exn. *)
  if is_none t.uncaught_exn
  then (
    let priority = execution_context.priority in
    let job_queue =
      match priority with
      | Normal -> t.normal_priority_jobs
      | Low -> t.low_priority_jobs
    in
    Job_queue.enqueue job_queue execution_context f a;
    match t.job_queued_hook with
    | None -> ()
    | Some f -> f priority)
;;

let enqueue_job t job ~free_job =
  let job_pool = t.job_pool in
  enqueue
    t
    (Pool.get job_pool job Pool.Slot.t0)
    (Pool.get job_pool job Pool.Slot.t1)
    (Pool.get job_pool job Pool.Slot.t2);
  if free_job then Pool.free t.job_pool job
;;

let handle_fired (time_source : _ Synchronous_time_source.T1.t) job_or_event =
  let open Job_or_event.Match in
  let (K k) = kind job_or_event in
  match k, project k job_or_event with
  | Job, job -> enqueue_job time_source.scheduler job ~free_job:true
  | Event, event -> Synchronous_time_source.fire time_source event
;;

let create () =
  let now = Time_ns.now () in
  let rec t =
    { check_access = None
    ; job_pool = Job_pool.create ()
    ; normal_priority_jobs = Job_queue.create ()
    ; low_priority_jobs = Job_queue.create ()
    ; very_low_priority_workers = Deque.create ()
    ; main_execution_context = Execution_context.main
    ; current_execution_context = Execution_context.main
    ; uncaught_exn = None
    ; cycle_start = now
    ; cycle_count = 0
    ; in_cycle = false
    ; run_every_cycle_start = []
    ; run_every_cycle_end = []
    ; last_cycle_time = sec 0.
    ; last_cycle_num_jobs = 0
    ; total_cycle_time = sec 0.
    ; time_source
    ; external_jobs = Thread_safe_queue.create ()
    ; thread_safe_external_job_hook = ignore
    ; job_queued_hook = None
    ; event_added_hook = None
    ; yield = Bvar.create ()
    ; yield_until_no_jobs_remain = Bvar.create () (* configuration *)
    ; check_invariants = Async_kernel_config.check_invariants
    ; max_num_jobs_per_priority_per_cycle =
        Async_kernel_config.max_num_jobs_per_priority_per_cycle
    ; record_backtraces = Async_kernel_config.record_backtraces
    ; on_start_of_cycle = Fn.id
    ; on_end_of_cycle = Fn.id
    }
  and events =
    Timing_wheel.create ~config:Async_kernel_config.timing_wheel_config ~start:now
  and time_source : _ Synchronous_time_source.T1.t =
    { id = Types.Time_source_id.create ()
    ; advance_errors = []
    ; am_advancing = false
    ; events
    ; handle_fired = (fun alarm -> handle_fired time_source (Alarm.value events alarm))
    ; fired_events = Event.none
    ; is_wall_clock = true
    ; most_recently_fired = Event.none
    ; scheduler = t
    }
  in
  t
;;

let is_dead t = is_some t.uncaught_exn
let set_check_access t f = t.check_access <- f

let t_ref =
  match Result.try_with create with
  | Ok t -> ref t
  | Error exn ->
    Debug.log "Async cannot create its raw scheduler" exn [%sexp_of: exn];
    exit 1
;;

let check_access t =
  match t.check_access with
  | None -> ()
  | Some f -> f ()
;;

let t () =
  let t = !t_ref in
  check_access t;
  t
;;

let current_execution_context t =
  if t.record_backtraces
  then Execution_context.record_backtrace t.current_execution_context
  else t.current_execution_context
;;

let with_execution_context t tmp_context ~f =
  let old_context = current_execution_context t in
  set_execution_context t tmp_context;
  protect ~f ~finally:(fun () -> set_execution_context t old_context)
;;

let create_job (type a) t execution_context f a =
  if Pool.is_full t.job_pool then t.job_pool <- Pool.grow t.job_pool;
  Pool.new3
    t.job_pool
    execution_context
    (Obj.magic (f : a -> unit) : Obj.t -> unit)
    (Obj.repr (a : a))
;;

let got_uncaught_exn t exn sexp =
  if debug then Debug.log "got_uncaught_exn" (exn, sexp) [%sexp_of: Exn.t * Sexp.t];
  List.iter [ t.normal_priority_jobs; t.low_priority_jobs ] ~f:Job_queue.clear;
  t.uncaught_exn <- Some (exn, sexp)
;;

(* [start_cycle t ~max_num_jobs_per_priority] enables subsequent calls of [run_jobs]
   to run up to [max_num_jobs_per_priority] jobs of each priority level. *)
let start_cycle t ~max_num_jobs_per_priority =
  let n = Max_num_jobs_per_priority_per_cycle.raw max_num_jobs_per_priority in
  Job_queue.set_jobs_left_this_cycle t.normal_priority_jobs n;
  Job_queue.set_jobs_left_this_cycle t.low_priority_jobs n
;;

(* [run_jobs t] removes jobs from [t] one at a time and runs them, stopping as soon
   as an unhandled exception is raised, or when no more jobs can be run at any priority,
   as per [~max_num_jobs_per_priority]. *)
let rec run_jobs t =
  match Job_queue.run_jobs t.normal_priority_jobs t with
  | Error _ as e -> e
  | Ok () ->
    (match Job_queue.run_jobs t.low_priority_jobs t with
     | Error _ as e -> e
     | Ok () ->
       if Job_queue.can_run_a_job t.normal_priority_jobs
       || Job_queue.can_run_a_job t.low_priority_jobs
       then run_jobs t
       else Ok ())
;;

let stabilize t =
  start_cycle
    t
    ~max_num_jobs_per_priority:
      (Max_num_jobs_per_priority_per_cycle.create_exn Int.max_value);
  match run_jobs t with
  | Ok () -> Ok ()
  | Error (exn, _backtrace) -> Error exn
;;

let create_time_source
      ?(timing_wheel_config = Async_kernel_config.timing_wheel_config)
      ~now
      ()
  =
  let t = t () in
  let events = Timing_wheel.create ~config:timing_wheel_config ~start:now in
  let rec time_source : _ Synchronous_time_source.T1.t =
    { id = Types.Time_source_id.create ()
    ; advance_errors = []
    ; am_advancing = false
    ; events
    ; handle_fired = (fun alarm -> handle_fired time_source (Alarm.value events alarm))
    ; fired_events = Event.none
    ; is_wall_clock = false
    ; most_recently_fired = Event.none
    ; scheduler = t
    }
  in
  time_source
;;

let wall_clock () = Synchronous_time_source.read_only (t ()).time_source
