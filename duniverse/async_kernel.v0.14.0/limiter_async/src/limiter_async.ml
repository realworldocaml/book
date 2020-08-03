open Core_kernel
open Async_kernel
open Limiter.Infinite_or_finite

module Outcome = struct
  type 'a t =
    | Ok of 'a
    | Aborted
    | Raised of exn
  [@@deriving sexp_of]
end

module Job = struct
  type t =
    | Immediate : Monitor.t * ('a -> unit) * 'a -> t
    | Deferred  : ('a -> 'b Deferred.t) * 'a * 'b Outcome.t Ivar.t -> t
end

module Expert = struct
  type t =
    { continue_on_error : bool
    (* [is_dead] is true if [t] was killed due to a job raising an exception or [kill t]
       being called. *)
    ; mutable is_dead       : bool
    (* Ivar that is filled the next time return_to_hopper is called. *)
    ; mutable hopper_filled : unit Ivar.t option
    ; limiter               : Limiter.t
    ; throttle_queue        : ((int * Job.t) Queue.t [@sexp.opaque]) }
  [@@deriving sexp_of]

  let to_jane_limiter t = t.limiter

  let cycle_start () = Async_kernel_scheduler.cycle_start_ns ()

  let create_exn
        ~hopper_to_bucket_rate_per_sec
        ~bucket_limit
        ~in_flight_limit
        ~initial_bucket_level
        ~initial_hopper_level
        ~continue_on_error
    =
    let limiter =
      Limiter.Expert.create_exn
        ~now:(cycle_start ())
        ~hopper_to_bucket_rate_per_sec
        ~bucket_limit
        ~in_flight_limit
        ~initial_bucket_level
        ~initial_hopper_level
    in
    let throttle_queue = Queue.create () in
    { continue_on_error
    ; is_dead       = false
    ; hopper_filled = None
    ; limiter
    ; throttle_queue
    }
  ;;

  let is_dead t = t.is_dead

  let kill_job = function
    | Job.Deferred  (_, _, i)       -> Ivar.fill_if_empty i Aborted
    | Job.Immediate (monitor, _, _) ->
      Monitor.send_exn monitor ~backtrace:`Get (Failure "Limiter killed")
  ;;

  let kill t =
    if not t.is_dead then begin
      t.is_dead <- true;
      Queue.iter t.throttle_queue ~f:(fun (_, job) -> kill_job job)
    end;
  ;;

  let saw_error t = if not t.continue_on_error then (kill t)

  let wait_for_hopper_fill t =
    match t.hopper_filled with
    | Some i -> Ivar.read i
    | None   ->
      let i = Ivar.create () in
      t.hopper_filled <- Some i;
      Ivar.read i
  ;;

  let return_to_hopper t ~now amount =
    begin match t.hopper_filled with
    | None   -> ()
    | Some i ->
      Ivar.fill i ();
      t.hopper_filled <- None;
    end;
    Limiter.Expert.return_to_hopper t.limiter ~now amount
  ;;

  let run_job_now t job ~return_after : unit =
    if t.is_dead
    then (kill_job job)
    else begin
      match job with
      | Job.Immediate (monitor, f, v)    ->
        begin try
          f v
        with
        | e -> Monitor.send_exn monitor ~backtrace:`Get e
        end;
        return_to_hopper t ~now:(cycle_start ()) return_after
      | Job.Deferred  (f, v, i) ->
        Monitor.try_with (fun () ->
          f v)
        >>> fun res ->
        return_to_hopper t ~now:(cycle_start ()) return_after;
        match res with
        | Error e ->
          Ivar.fill_if_empty i (Raised e);
          saw_error t
        | Ok v    ->
          Ivar.fill_if_empty i (Ok v)
    end
  ;;

  (* given a job, immediately creates and runs a job that fails with the given (as a
     format string) message *)
  let fail_job t job k =
    ksprintf (fun s ->
      let f () = failwith s in
      let job =
        match job with
        | Job.Immediate (monitor, _, _) -> Job.Immediate (monitor, f, ())
        | Job.Deferred (_, _, i)        -> Job.Deferred (f, (), i)
      in
      run_job_now t job ~return_after:0)
      k
  ;;

  let rec run_throttled_jobs_until_empty t =
    if Queue.length t.throttle_queue = 0
    then ()
    else begin
      let amount, job = Queue.peek_exn t.throttle_queue in
      let now         = cycle_start () in
      match Limiter.Expert.try_take t.limiter ~now amount with
      | Asked_for_more_than_bucket_limit ->
        fail_job t job !"job asked for more tokens (%i) than possible (%i)"
          amount (Limiter.bucket_limit t.limiter);
        run_throttled_jobs_until_empty t
      | Taken ->
        (* Safe, because we checked the length above.  And, we're guaranteed that
           dequeue_exn gets out the same job that peek_exn does.  *)
        ignore (Queue.dequeue_exn t.throttle_queue : (int * Job.t));
        run_job_now t job ~return_after:amount;
        run_throttled_jobs_until_empty t
      | Unable ->
        begin match Limiter.Expert.tokens_may_be_available_when t.limiter ~now amount with
        | Never_because_greater_than_bucket_limit ->
          fail_job t job !"job asked for more tokens (%i) than possible (%i)"
            amount (Limiter.bucket_limit t.limiter);
          run_throttled_jobs_until_empty t
        | When_return_to_hopper_is_called   ->
          wait_for_hopper_fill t
          >>> fun () ->
          run_throttled_jobs_until_empty t
        | At expected_fill_time ->
          let min_fill_time =
            Time_ns.add (cycle_start ()) (Async_kernel_scheduler.event_precision_ns ())
          in
          Clock_ns.at (Time_ns.max expected_fill_time min_fill_time)
          >>> fun () ->
          run_throttled_jobs_until_empty t
        end
    end
  ;;

  let enqueue_job_and_maybe_start_queue_runner t amount job ~allow_immediate_run =
    let bucket_limit = Limiter.bucket_limit t.limiter in
    if bucket_limit < amount
    then (fail_job t job !"requested job size (%i) exceeds the possible size (%i)"
            amount bucket_limit);
    if t.is_dead
    then (kill_job job)
    else begin
      if Queue.length t.throttle_queue > 0
      then (Queue.enqueue t.throttle_queue (amount, job))
      else begin
        let now = cycle_start () in
        match Limiter.Expert.try_take t.limiter ~now amount with
        | Asked_for_more_than_bucket_limit ->
          fail_job t job !"requested job size (%i) exceeds the possible size (%i)"
            amount bucket_limit;
        | Taken  ->
          (* These semantics are copied from the current Throttle, and it was
             important enough there to add a specific unit test.  If you have

             do_f ();
             enqueue thing_to_do_later;
             do_g ();

             it is surprising if any portion of the closure thing_to_do_later happens, so
             we always schedule the work for later on the Async queue.

             This isn't as efficient as it could be for immediate jobs and can be avoided
             with [run_or_enqueue].
          *)
          if allow_immediate_run
          then (run_job_now t job ~return_after:amount)
          else (Async_kernel_scheduler.enqueue_job
                  Execution_context.main
                  (fun t ->
                     run_job_now t job ~return_after:amount) t)
        | Unable ->
          Queue.enqueue t.throttle_queue (amount, job);
          run_throttled_jobs_until_empty t
      end
    end
  ;;

  let enqueue_exn t ?(allow_immediate_run=false) amount f v =
    enqueue_job_and_maybe_start_queue_runner t amount ~allow_immediate_run
      (Immediate (Monitor.current (), f, v))
  ;;

  let enqueue' t amount f v =
    Deferred.create (fun i ->
      try
        enqueue_job_and_maybe_start_queue_runner t amount (Deferred (f, v, i))
          ~allow_immediate_run:false
      with e -> Ivar.fill i (Raised e))
  ;;

  let cost_of_jobs_waiting_to_start t =
    Queue.fold t.throttle_queue ~init:0
      ~f:(fun sum (cost, _) -> cost + sum)
  ;;

end

open Expert
type t = Expert.t [@@deriving sexp_of]
type limiter = t [@@deriving sexp_of]

module Common = struct
  let to_limiter (t:t) = t
  let kill = Expert.kill
  let is_dead = Expert.is_dead
end

module type Common = sig
  type _ t

  (** kills [t], which aborts all enqueued jobs that haven't started and all jobs enqueued
      in the future.  If [t] has already been killed, then calling [kill t] has no effect.
      Note that kill does not effect currently running jobs in any way. *)
  val kill : _ t -> unit

  (** [is_dead t] returns [true] if [t] was killed, either by [kill] or by an unhandled
      exception in a job. *)
  val is_dead : _ t -> bool

  val to_limiter : _ t -> limiter
end

module Token_bucket = struct
  type t = limiter [@@deriving sexp_of]
  type _ u = t

  let create_exn
        ~burst_size:bucket_limit
        ~sustained_rate_per_sec:fill_rate
        ~continue_on_error
        ?in_flight_limit
        ?(initial_burst_size = 0)
        ()
    =
    let in_flight_limit =
      match in_flight_limit with
      | None       -> Infinite
      | Some limit -> Finite limit
    in
    Expert.create_exn
      ~bucket_limit
      ~in_flight_limit
      ~hopper_to_bucket_rate_per_sec:(Finite fill_rate)
      ~initial_bucket_level:initial_burst_size
      ~initial_hopper_level:Infinite
      ~continue_on_error
  ;;

  let enqueue_exn        = Expert.enqueue_exn
  let enqueue'           = Expert.enqueue'

  include Common
end

module Throttle = struct
  type t = limiter
  [@@deriving sexp_of]
  type _ u = t

  let create_exn
        ~concurrent_jobs_target
        ~continue_on_error
        ?burst_size
        ?sustained_rate_per_sec
        ()
    =
    if concurrent_jobs_target < 1
    then (failwithf !"concurrent_jobs_target < 1 (%i) doesn't make sense"
            concurrent_jobs_target ());
    let concurrent_jobs_target = concurrent_jobs_target in
    let hopper_to_bucket_rate_per_sec =
      match sustained_rate_per_sec with
      | None      -> Infinite
      | Some rate -> Finite rate
    in
    let bucket_limit =
      match burst_size with
      | None            -> concurrent_jobs_target
      | Some burst_size -> burst_size
    in
    let initial_bucket_level = bucket_limit in
    Expert.create_exn
      ~bucket_limit
      ~in_flight_limit:(Finite concurrent_jobs_target)
      ~hopper_to_bucket_rate_per_sec
      ~initial_bucket_level
      ~initial_hopper_level:(Finite 0)
      ~continue_on_error
  ;;

  let enqueue_exn t ?allow_immediate_run f v =
    Expert.enqueue_exn t ?allow_immediate_run 1 f v
  ;;

  let enqueue' t f v =
    Expert.enqueue' t 1 f v

  let jlimiter = Expert.to_jane_limiter

  let concurrent_jobs_target t =
    jlimiter t
    |> Limiter.bucket_limit
  ;;

  let num_jobs_waiting_to_start t = Queue.length t.throttle_queue

  let num_jobs_running t =
    Limiter.in_flight (jlimiter t) ~now:(Async_kernel_scheduler.cycle_start_ns ())
  ;;

  include Common
end

module Sequencer = struct
  include Throttle

  let create ?(continue_on_error=false) ?burst_size ?sustained_rate_per_sec () =
    create_exn ~concurrent_jobs_target:1
      ~continue_on_error ?burst_size ?sustained_rate_per_sec ()
  ;;

  include Common
end

module Resource_throttle = struct
  type 'a t =
    { throttle  : Throttle.t
    ; resources : 'a Queue.t }
  [@@deriving sexp_of]

  let create_exn ~resources ~continue_on_error ?burst_size ?sustained_rate_per_sec () =
    let resources = Queue.of_list resources in
    let max_concurrent_jobs = Queue.length resources in
    let throttle =
      Throttle.create_exn
        ~concurrent_jobs_target:max_concurrent_jobs
        ~continue_on_error
        ?burst_size
        ?sustained_rate_per_sec
        ()
    in
    { throttle; resources }
  ;;

  let enqueue_gen t ?allow_immediate_run f enqueue =
    let f () =
      let v = Queue.dequeue_exn t.resources in
      protect ~f:(fun () -> f v)
        ~finally:(fun () -> Queue.enqueue t.resources v)
    in
    enqueue t.throttle ?allow_immediate_run f ()
  ;;

  let enqueue_exn t ?allow_immediate_run f =
    enqueue_gen t ?allow_immediate_run f Throttle.enqueue_exn
  ;;

  let enqueue' t f =
    let f () =
      let v = Queue.dequeue_exn t.resources in
      Monitor.protect (fun () -> f v)
        ~finally:(fun () -> Queue.enqueue t.resources v; Deferred.unit)
    in
    Throttle.enqueue' t.throttle f ()
  ;;

  let max_concurrent_jobs t = Throttle.concurrent_jobs_target t.throttle

  let to_limiter t = t.throttle
  let kill       t = kill    t.throttle
  let is_dead    t = is_dead t.throttle
end

