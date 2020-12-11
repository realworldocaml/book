open Core
open Import
open Raw_scheduler

let debug = Debug.thread_safe

let run_holding_async_lock
      (type a b)
      ?(wakeup_scheduler = true)
      t
      (f : unit -> a)
      ~(finish : (a, exn) Result.t -> b)
  : b
  =
  if debug then Debug.log "run_holding_async_lock" t [%sexp_of: t];
  if not (am_holding_lock t) then lock t;
  protect
    ~finally:(fun () ->
      if wakeup_scheduler then thread_safe_wakeup_scheduler t;
      unlock t)
    ~f:(fun () ->
      (* We run [f] within the [main_execution_context] so that any errors are sent to its
         monitor, rather than whatever random monitor happened to be in effect. *)
      finish
        (with_execution_context t Kernel_scheduler.main_execution_context ~f:(fun () ->
           Result.try_with f)))
;;

let ensure_in_a_thread t function_ =
  if is_main_thread ()
  then raise_s [%message "cannot call from the main thread" (function_ : string)];
  if am_holding_lock t
  then raise_s [%message "cannot call while holding the async lock" (function_ : string)]
;;

let run_in_async_with_optional_cycle ?wakeup_scheduler t f =
  if debug then Debug.log "run_in_async_with_optional_cycle" t [%sexp_of: t];
  ensure_in_a_thread t "run_in_async_with_optional_cycle";
  run_holding_async_lock ?wakeup_scheduler t f ~finish:(function
    | Error exn -> Error exn
    | Ok (maybe_run_a_cycle, a) ->
      (match maybe_run_a_cycle with
       | `Do_not_run_a_cycle -> ()
       | `Run_a_cycle -> have_lock_do_cycle t);
      Ok a)
;;

let block_on_async t f =
  if debug then Debug.log "block_on_async" t [%sexp_of: t];
  (* We disallow calling [block_on_async] if the caller is running inside async.  This can
     happen if one is the scheduler, or if one is in some other thread that has used, e.g.
     [run_in_async] to call into async and run a cycle.  We do however, want to allow the
     main thread to call [block_on_async], in which case it should release the lock and
     allow the scheduler, which is running in another thread, to run. *)
  if i_am_the_scheduler t || (am_holding_lock t && not (is_main_thread ()))
  then raise_s [%message "called [block_on_async] from within async"];
  (* While [block_on_async] is blocked, the Async scheduler may run and set the execution
     context.  So we save and restore the execution context, if we're in the main thread.
     The restoration is necessary because subsequent code in the main thread can do
     operations that rely on the execution context. *)
  let execution_context =
    Kernel_scheduler.current_execution_context t.kernel_scheduler
  in
  (* Create a scheduler thread if the scheduler isn't already running. *)
  if not t.is_running
  then (
    t.is_running <- true;
    (* Release the Async lock if necessary, so that the scheduler can acquire it. *)
    if am_holding_lock t then unlock t;
    let scheduler_ran_a_job = Thread_safe_ivar.create () in
    upon (return ()) (fun () -> Thread_safe_ivar.fill scheduler_ran_a_job ());
    ignore
      (Core.Thread.create
         ~on_uncaught_exn:`Print_to_stderr
         (fun () ->
            Exn.handle_uncaught ~exit:true (fun () ->
              lock t;
              never_returns (be_the_scheduler t)))
         ()
       : Core.Thread.t);
    (* Block until the scheduler has run the above job. *)
    Thread_safe_ivar.read scheduler_ran_a_job);
  let maybe_blocked =
    run_holding_async_lock
      t
      (fun () -> Monitor.try_with f ~name:"block_on_async")
      ~finish:(fun res ->
        match res with
        | Error exn -> `Available (Error exn)
        | Ok d ->
          (match Deferred.peek d with
           | Some v -> `Available v
           | None ->
             have_lock_do_cycle t;
             (match Deferred.peek d with
              | Some v -> `Available v
              | None ->
                let q = Squeue.create 1 in
                upon d (fun v -> Squeue.push_uncond q v);
                (* Squeue.pop can block, so we have to do it outside async *)
                `Blocked_wait_on_squeue q)))
  in
  let res =
    match maybe_blocked with
    | `Available v -> v
    | `Blocked_wait_on_squeue q ->
      (* [run_holding_async_lock] released the lock.  If the scheduler wasn't already
         running when [block_on_async] was called, then we started it above.  So, the
         scheduler is running, and will eventually run the job to put something on the
         squeue.  So, it's OK to block waiting for it. *)
      Squeue.pop q
  in
  (* If we're the main thread, we should lock the scheduler for the rest of main, to
     prevent the scheduler, which is now running in another thread, from interfering with
     the main thread.  We also restore the execution context, so that the code in the main
     thread will be in the same execution context as before it called [block_on_async].
     The restored execution context will usually be [Execution_context.main], but need not
     be, if the user has done operations that adjust the current execution context,
     e.g. [Monitor.within].  If we're not in the main thread, the we don't need to
     and cannot restore the execution context, because we do not hold the Async lock. *)
  if is_main_thread ()
  then (
    lock t;
    (* While [block_on_async] is blocked, Async can run and set the execution context.  So
       we restore the execution context, if we're in the main thread.  The restoration is
       necessary because subsequent code in the main thread can do operations that rely on
       the execution context. *)
    Kernel_scheduler.set_execution_context t.kernel_scheduler execution_context);
  res
;;

let block_on_async_exn t f = Result.ok_exn (block_on_async t f)

let reset_scheduler t =
  if debug then Debug.log "reset_scheduler" t [%sexp_of: t];
  if i_am_the_scheduler t || (am_holding_lock t && not (is_main_thread ()))
  then raise_s [%message "called [reset_scheduler] from within async"];
  if am_holding_lock t then unlock t;
  Raw_scheduler.thread_safe_reset ()
;;

let run_in_async ?wakeup_scheduler t f =
  if debug then Debug.log "run_in_async" t [%sexp_of: t];
  ensure_in_a_thread t "run_in_async";
  run_holding_async_lock ?wakeup_scheduler t f ~finish:Fn.id
;;

let run_in_async_exn ?wakeup_scheduler t f =
  Result.ok_exn (run_in_async ?wakeup_scheduler t f)
;;

let run_in_async_wait t f =
  if debug then Debug.log "run_in_async_wait" t [%sexp_of: t];
  ensure_in_a_thread t "run_in_async_wait";
  block_on_async t f
;;

let run_in_async_wait_exn t f = Result.ok_exn (run_in_async_wait t f)

let deferred t =
  let ivar =
    if am_holding_lock t
    then Ivar.create ()
    else run_holding_async_lock t Ivar.create ~finish:Result.ok_exn
  in
  let fill x = run_in_async_exn t (fun () -> Ivar.fill ivar x) in
  Ivar.read ivar, fill
;;

let t () = the_one_and_only ~should_lock:false
let am_holding_async_lock () = am_holding_lock (t ())
let deferred () = deferred (t ())

let run_in_async_with_optional_cycle ?wakeup_scheduler f =
  run_in_async_with_optional_cycle ?wakeup_scheduler (t ()) f
;;

let run_in_async ?wakeup_scheduler f = run_in_async ?wakeup_scheduler (t ()) f
let run_in_async_exn ?wakeup_scheduler f = run_in_async_exn ?wakeup_scheduler (t ()) f
let block_on_async f = block_on_async (t ()) f
let block_on_async_exn f = block_on_async_exn (t ()) f
let run_in_async_wait f = run_in_async_wait (t ()) f
let run_in_async_wait_exn f = run_in_async_wait_exn (t ()) f
let reset_scheduler () = reset_scheduler (t ())
