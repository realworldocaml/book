open Core
open Import
open Raw_scheduler
module Priority = Linux_ext.Priority

module When_finished = struct
  type t =
    | Notify_the_scheduler
    | Take_the_async_lock
    | Try_to_take_the_async_lock
  [@@deriving enumerate, sexp_of]

  let default = ref Try_to_take_the_async_lock
end

let run_after_scheduler_is_started
      ~priority
      ~thread
      ~(when_finished : When_finished.t)
      ~name
      ~t
      f
  =
  let ivar = Ivar.create () in
  let doit () =
    (* At this point, we are in a thread-pool thread, not the async thread. *)
    let result = Result.try_with f in
    let locked =
      match when_finished with
      | Take_the_async_lock ->
        lock t;
        true
      | Notify_the_scheduler -> false
      | Try_to_take_the_async_lock ->
        (match thread_pool_cpu_affinity t with
         | Inherit -> try_lock t
         | Cpuset _ ->
           (* If the user specified an affinity for the thread pool, they presumably intend
              for Async jobs to be affinitized differently from thread-pool threads, so we
              don't even attempt to run jobs on the thread-pool thread. *)
           false)
    in
    if locked
    then
      protect
        ~finally:(fun () -> unlock t)
        ~f:(fun () ->
          Ivar.fill ivar result;
          have_lock_do_cycle t)
    else
      thread_safe_enqueue_external_job
        t
        (current_execution_context t)
        (fun () -> Ivar.fill ivar result)
        ()
  in
  (match thread with
   | None ->
     ok_exn (Thread_pool.add_work t.thread_pool doit ?name ?priority);
     if Thread_pool.num_threads t.thread_pool = 0
     then
       raise_s
         [%message
           "Async's thread pool was unable to create a single thread"
             ~_:
               (Thread_pool.last_thread_creation_failure t.thread_pool
                : (Sexp.t option[@sexp.option]))]
   | Some helper_thread ->
     ok_exn
       (Thread_pool.add_work_for_helper_thread
          t.thread_pool
          helper_thread
          doit
          ?name
          ?priority));
  Ivar.read ivar >>| Result.ok_exn
;;

let run ?priority ?thread ?(when_finished = !When_finished.default) ?name f =
  match !Raw_scheduler.the_one_and_only_ref with
  | Initialized t when t.is_running ->
    run_after_scheduler_is_started ~priority ~thread ~when_finished ~name ~t f
  | _ ->
    (* We use [bind unit ...] to force calls to [run_after_scheduler_is_started] to wait
       until after the scheduler is started.  We do this because
       [run_after_scheduler_is_started] will cause things to run in other threads, and
       when a job is finished in another thread, it will try to acquire the async lock and
       manipulate async datastructures.  This seems hard to think about if async hasn't
       even started yet. *)
    Deferred.bind (return ()) ~f:(fun () ->
      run_after_scheduler_is_started
        ~priority
        ~thread
        ~when_finished
        ~name
        ~t:(Raw_scheduler.t ())
        f)
;;

module Helper_thread = struct
  (* A wrapper around [Thread_pool]'s helper thread, so we can attach a finalizer. *)
  type t = { thread_pool_helper_thread : Thread_pool.Helper_thread.t }
  [@@deriving fields, sexp_of]

  (* Both [create] and [create_now] add Async finalizers to the returned helper thread so
     that the thread can be added back to the set of worker threads when there are no
     references to the helper thread and the thread has no pending work.  Because
     [Thread_pool.finished_with_helper_thread] needs to acquire the thread pool lock, it
     cannot be run within an ordinary finalizer, since that could cause it to be run in a
     context where the code interrupted by the GC might already be holding the thread pool
     lock, which would result in a deadlock.  Hence we use an Async finalizer -- this
     causes the GC to merely schedule an Async job that calls
     [Thread_pool.finished_with_helper_thread].  We don't attach the finalizer inside
     [Thread_pool] because the thread pool doesn't know about Async, and in particular
     doesn't know about Async finalizers. *)
  let create_internal scheduler thread_pool_helper_thread =
    let finalize { thread_pool_helper_thread } =
      Thread_pool.finished_with_helper_thread
        scheduler.thread_pool
        thread_pool_helper_thread
    in
    let t = { thread_pool_helper_thread } in
    add_finalizer_exn scheduler t finalize;
    t
  ;;

  let create_now ?priority ?name () =
    let scheduler = the_one_and_only ~should_lock:true in
    Result.map
      (Thread_pool.create_helper_thread scheduler.thread_pool ?name ?priority)
      ~f:(fun helper_thread -> create_internal scheduler helper_thread)
  ;;

  let create ?priority ?name () =
    let scheduler = the_one_and_only ~should_lock:true in
    let%map helper_thread =
      run (fun () ->
        Thread_pool.become_helper_thread scheduler.thread_pool ?name ?priority)
    in
    create_internal scheduler (ok_exn helper_thread)
  ;;
end

let run ?priority ?thread ?when_finished ?name f =
  let thread = Option.map thread ~f:Helper_thread.thread_pool_helper_thread in
  run ?priority ?thread ?when_finished ?name f
;;

let syscall ~name f = run ~name (fun () -> Syscall.syscall f)
let syscall_exn ~name f = run ~name (fun () -> Result.ok_exn (Syscall.syscall f))

let pipe_of_squeue sq =
  let r, w = Pipe.create () in
  (* The functions are defined to avoid unnecessary allocation. *)
  let pull () =
    let q = Linked_queue.create () in
    Squeue.transfer_queue sq q;
    q
  in
  let rec continue q =
    Linked_queue.iter q ~f:(Pipe.write_without_pushback w);
    Pipe.pushback w >>> loop
  (* [run pull] runs [pull] in a thread, because [Squeue.transfer_queue] can block. *)
  and loop () = run pull >>> continue in
  loop ();
  r
;;
