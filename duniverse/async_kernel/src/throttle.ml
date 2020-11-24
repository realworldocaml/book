open! Core_kernel
open! Import
open! Deferred_std
module Deferred = Deferred1

type 'a outcome =
  [ `Ok of 'a
  | `Aborted
  | `Raised of exn
  ]
[@@deriving sexp_of]

module Internal_job : sig
  type 'a t [@@deriving sexp_of]

  val create : ('a -> 'b Deferred.t) -> 'a t * 'b outcome Deferred.t

  (* Every internal job will eventually be either [run] or [abort]ed, but not both. *)

  val run : 'a t -> 'a -> [ `Ok | `Raised ] Deferred.t
  val abort : _ t -> unit
end = struct
  type 'a t =
    { start : [ `Abort | `Start of 'a ] Ivar.t
    ; outcome : [ `Ok | `Aborted | `Raised ] Deferred.t
    }
  [@@deriving sexp_of]

  let create work =
    let start = Ivar.create () in
    let result =
      match%bind Ivar.read start with
      | `Abort -> return `Aborted
      | `Start a ->
        (match%map Monitor.try_with (fun () -> work a) with
         | Ok a -> `Ok a
         | Error exn -> `Raised exn)
    in
    let outcome =
      match%map result with
      | `Ok _ -> `Ok
      | `Aborted -> `Aborted
      | `Raised _ -> `Raised
    in
    let t = { start; outcome } in
    t, result
  ;;

  let run t a =
    Ivar.fill t.start (`Start a);
    match%map t.outcome with
    | `Aborted -> assert false
    | (`Ok | `Raised) as x -> x
  ;;

  let abort t = Ivar.fill t.start `Abort
end

type 'a t =
  { continue_on_error : bool
  ; max_concurrent_jobs : int
  ; (* [job_resources_not_in_use] holds resources that are not currently in use by a
       running job. *)
    job_resources_not_in_use : 'a Stack_or_counter.t
  ;
    (* [jobs_waiting_to_start] is the queue of jobs that haven't yet started. *)
    jobs_waiting_to_start : 'a Internal_job.t Queue.t
  ; (* [0 <= num_jobs_running <= max_concurrent_jobs]. *)
    mutable num_jobs_running : int
  ; (* [capacity_available] is [Some ivar] if user code has called [capacity_available t]
       and is waiting to be notified when capacity is available in the throttle.
       [maybe_start_job] will fill [ivar] when capacity becomes available, i.e. when
       [jobs_waiting_to_start] is empty and [num_jobs_running < max_concurrent_jobs]. *)
    mutable capacity_available : unit Ivar.t option
  ; (* [is_dead] is true if [t] was killed due to a job raising an exception or [kill t]
       being called. *)
    mutable is_dead : bool
  ; (* [cleans] holds functions that will be called to clean each resource when [t] is
       killed. *)
    mutable cleans : ('a -> unit Deferred.t) list
  ; (* [num_resources_not_cleaned] is the number of resources whose clean functions have
       not yet completed.  While [t] is alive, [num_resources_not_cleaned =
       max_concurrent_jobs].  Once [t] is killed, [num_resources_not_cleaned] decreases to
       zero over time as the clean functions complete. *)
    mutable num_resources_not_cleaned : int
  ; (* [cleaned] becomes determined when [num_resources_not_cleaned] reaches zero,
       i.e. after [t] is killed and all its clean functions complete. *)
    cleaned : unit Ivar.t
  }
[@@deriving fields, sexp_of]

let invariant invariant_a t : unit =
  try
    let check f field = f (Field.get field t) in
    Fields.iter
      ~continue_on_error:ignore
      ~max_concurrent_jobs:
        (check (fun max_concurrent_jobs -> assert (max_concurrent_jobs > 0)))
      ~job_resources_not_in_use:
        (check (fun job_resources_not_in_use ->
           Stack_or_counter.iter job_resources_not_in_use ~f:invariant_a;
           assert (
             Stack_or_counter.length job_resources_not_in_use
             = if t.is_dead then 0 else t.max_concurrent_jobs - t.num_jobs_running)))
      ~jobs_waiting_to_start:
        (check (function jobs_waiting_to_start ->
           if t.is_dead then assert (Queue.is_empty jobs_waiting_to_start)))
      ~num_jobs_running:
        (check (fun num_jobs_running ->
           assert (num_jobs_running >= 0);
           assert (num_jobs_running <= t.max_concurrent_jobs);
           if num_jobs_running < t.max_concurrent_jobs
           then assert (Queue.is_empty t.jobs_waiting_to_start)))
      ~capacity_available:
        (check (function
           | None -> ()
           | Some ivar -> assert (Ivar.is_empty ivar)))
      ~is_dead:ignore
      ~cleans:ignore
      ~num_resources_not_cleaned:
        (check (fun num_resources_not_cleaned ->
           assert (num_resources_not_cleaned >= 0);
           assert (num_resources_not_cleaned <= t.max_concurrent_jobs);
           if num_resources_not_cleaned < t.max_concurrent_jobs then assert t.is_dead))
      ~cleaned:
        (check (fun cleaned ->
           if Ivar.is_full cleaned then assert (t.num_resources_not_cleaned = 0)))
  with
  | exn -> raise_s [%message "Throttle.invariant failed" (exn : exn) (t : _ t)]
;;

module T2 = struct
  type nonrec ('a, 'kind) t = 'a t [@@deriving sexp_of]

  let invariant invariant_a _ t = invariant invariant_a t
end

let num_jobs_waiting_to_start t = Queue.length t.jobs_waiting_to_start

let clean_resource t a =
  Deferred.all_unit (List.map t.cleans ~f:(fun f -> f a))
  >>> fun () ->
  t.num_resources_not_cleaned <- t.num_resources_not_cleaned - 1;
  if t.num_resources_not_cleaned = 0 then Ivar.fill t.cleaned ()
;;

let kill t =
  if not t.is_dead
  then (
    t.is_dead <- true;
    Queue.iter t.jobs_waiting_to_start ~f:Internal_job.abort;
    Queue.clear t.jobs_waiting_to_start;
    Stack_or_counter.iter t.job_resources_not_in_use ~f:(fun a -> clean_resource t a);
    Stack_or_counter.clear t.job_resources_not_in_use)
;;

let at_kill t f =
  (* We preserve the execution context so that exceptions raised by [f] go to the monitor
     in effect when [at_kill] was called. *)
  let f = unstage (Monitor.Exported_for_scheduler.preserve_execution_context' f) in
  t.cleans <- f :: t.cleans
;;

let cleaned t = Ivar.read t.cleaned

let rec start_job t =
  assert (not t.is_dead);
  assert (t.num_jobs_running < t.max_concurrent_jobs);
  assert (not (Queue.is_empty t.jobs_waiting_to_start));
  let job = Queue.dequeue_exn t.jobs_waiting_to_start in
  t.num_jobs_running <- t.num_jobs_running + 1;
  let job_resource = Stack_or_counter.pop_exn t.job_resources_not_in_use in
  Internal_job.run job job_resource
  >>> fun res ->
  t.num_jobs_running <- t.num_jobs_running - 1;
  (match res with
   | `Ok -> ()
   | `Raised -> if not t.continue_on_error then kill t);
  if t.is_dead
  then clean_resource t job_resource
  else (
    Stack_or_counter.push t.job_resources_not_in_use job_resource;
    if not (Queue.is_empty t.jobs_waiting_to_start)
    then start_job t
    else (
      match t.capacity_available with
      | None -> ()
      | Some ivar ->
        Ivar.fill ivar ();
        t.capacity_available <- None))
;;

let create_internal ~continue_on_error job_resources =
  let max_concurrent_jobs = Stack_or_counter.length job_resources in
  { continue_on_error
  ; max_concurrent_jobs
  ; job_resources_not_in_use = job_resources
  ; jobs_waiting_to_start = Queue.create ()
  ; num_jobs_running = 0
  ; capacity_available = None
  ; is_dead = false
  ; cleans = []
  ; num_resources_not_cleaned = max_concurrent_jobs
  ; cleaned = Ivar.create ()
  }
;;

let create_with ~continue_on_error job_resources =
  create_internal ~continue_on_error (Stack_or_counter.of_list job_resources)
;;

module Sequencer = struct
  type nonrec 'a t = 'a t [@@deriving sexp_of]

  let create ?(continue_on_error = false) a = create_with ~continue_on_error [ a ]
end

let create ~continue_on_error ~max_concurrent_jobs =
  if max_concurrent_jobs <= 0
  then
    raise_s
      [%message
        "Throttle.create requires positive max_concurrent_jobs, but got"
          (max_concurrent_jobs : int)];
  create_internal
    ~continue_on_error
    (Stack_or_counter.create_counter ~length:max_concurrent_jobs)
;;

module Job = struct
  type ('a, 'b) t =
    { internal_job : 'a Internal_job.t
    ; result : [ `Ok of 'b | `Aborted | `Raised of exn ] Deferred.t
    }

  let result t = t.result
  let abort t = Internal_job.abort t.internal_job

  let create f =
    let internal_job, result = Internal_job.create f in
    { internal_job; result }
  ;;
end

let enqueue' t f =
  let job = Job.create f in
  if t.is_dead
  then Job.abort job
  else (
    Queue.enqueue t.jobs_waiting_to_start job.internal_job;
    if t.num_jobs_running < t.max_concurrent_jobs then start_job t);
  Job.result job
;;

let handle_enqueue_result result =
  match result with
  | `Ok a -> a
  | `Aborted -> raise_s [%message "throttle aborted job"]
  | `Raised exn -> raise exn
;;

let enqueue t f = enqueue' t f >>| handle_enqueue_result

let enqueue_exclusive t f =
  let n = t.max_concurrent_jobs in
  if Int.( >= ) n 1_000_000
  then
    raise_s
      [%sexp
        "[enqueue_exclusive] was called with a very large value of \
         [max_concurrent_jobs]. This doesn't work."];
  let done_ = Ivar.create () in
  assert (n > 0);
  let f_placeholder _slot = Ivar.read done_ in
  for _ = 1 to n - 1 do
    don't_wait_for (enqueue t f_placeholder)
  done;
  let%map result = enqueue' t (fun _slot -> f ()) in
  Ivar.fill done_ ();
  handle_enqueue_result result
;;

let monad_sequence_how ?(how = `Sequential) ~f =
  stage
    (match how with
     | `Parallel -> f
     | (`Sequential | `Max_concurrent_jobs _) as how ->
       let max_concurrent_jobs =
         match how with
         | `Sequential -> 1
         | `Max_concurrent_jobs max_concurrent_jobs -> max_concurrent_jobs
       in
       let t = create ~continue_on_error:false ~max_concurrent_jobs in
       fun a -> enqueue t (fun () -> f a))
;;

let monad_sequence_how2 ?(how = `Sequential) ~f =
  stage
    (match how with
     | `Parallel -> f
     | (`Sequential | `Max_concurrent_jobs _) as how ->
       let max_concurrent_jobs =
         match how with
         | `Sequential -> 1
         | `Max_concurrent_jobs max_concurrent_jobs -> max_concurrent_jobs
       in
       let t = create ~continue_on_error:false ~max_concurrent_jobs in
       fun a1 a2 -> enqueue t (fun () -> f a1 a2))
;;

let prior_jobs_done t =
  (* We queue [t.max_concurrent_jobs] dummy jobs and when they are all started we know
     that all prior jobs finished.  We make sure that all dummy jobs wait for the last one
     to get started before finishing. *)
  Deferred.create (fun all_dummy_jobs_running ->
    let dummy_jobs_running = ref 0 in
    for _ = 1 to t.max_concurrent_jobs do
      don't_wait_for
        (enqueue t (fun _ ->
           incr dummy_jobs_running;
           if !dummy_jobs_running = t.max_concurrent_jobs
           then Ivar.fill all_dummy_jobs_running ();
           Ivar.read all_dummy_jobs_running))
    done)
;;

let capacity_available t =
  if num_jobs_running t < max_concurrent_jobs t
  then return ()
  else (
    match t.capacity_available with
    | Some ivar -> Ivar.read ivar
    | None -> Deferred.create (fun ivar -> t.capacity_available <- Some ivar))
;;
