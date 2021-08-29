open! Core
open! Import
module Cpu_affinity = Thread_pool_cpu_affinity

let debug_log message a sexp_of_a =
  eprintf
    "%s\n%!"
    (Sexp.to_string_hum [%sexp (Time_ns.now () : Time_ns.t), (message : string), (a : a)])
;;

module Time_ns = struct
  include Time_ns

  let sexp_of_t t = [%sexp (if am_running_test then epoch else t : t)]
end

module Thread_id : sig
  type t [@@deriving sexp_of]

  include Hashable with type t := t

  val of_ocaml_thread : Core.Thread.t -> t
  val self : unit -> t
end = struct
  include Int

  let of_ocaml_thread = Core.Thread.id
  let self () = of_ocaml_thread (Core.Thread.self ())
end

module Priority = Linux_ext.Priority

let priority_zero = Priority.of_int 0

let getpriority =
  match Linux_ext.getpriority with
  | Error _ -> const priority_zero
  | Ok f -> f
;;

let setpriority =
  match Linux_ext.setpriority with
  | Error _ -> Fn.ignore
  | Ok f -> f
;;

let set_thread_name =
  match Linux_ext.pr_set_name_first16 with
  | Ok f -> f
  | Error _ -> Fn.ignore
;;

let debug = ref false

(* We define everything in an [Internal] module, and then wrap in a
   [Mutex.critical_section] each thread-safe function exposed in the mli.

   When reading code here, keep in mind that there are two entry points:

   (1) The functions that are exposed for external consumption in the mli (these are
   protected by the mutex).

   (2) Code that is called within threads created in this module.  All such code should
   acquire the mutex before it affects the thread state. *)

module Internal = struct
  module Mutex = Nano_mutex

  let check_invariant = ref false
  let error = Or_error.error

  module Pool_id : Unique_id = Unique_id.Int63 ()

  module Work = struct
    type t =
      { (* When this work starts running, the name of the thread will be set (via
           [Linux_ext.pr_set_name]) to[name]. *)
        name : string
      ; doit : unit -> unit
      ; priority : Priority.t
      }
    [@@deriving sexp_of]
  end

  module Work_queue = struct
    type elt =
      | Stop
      | Work of Work.t
    [@@deriving sexp_of]

    type t = elt Squeue.t [@@deriving sexp_of]

    let create () = Squeue.create 1
    let enqueue t work = Squeue.push_uncond t work
  end

  module Helper_thread = struct
    type 'thread t =
      { in_pool : Pool_id.t
      ; mutable state : [ `In_use | `Finishing | `Finished ]
      ; thread : 'thread
      (* [default_name] will be used as the name of work run by the helper thread,
         unless that work is added with an overriding name. *)
      ; default_name : string
      (* [default_priority] will be used as the priority of work run by the helper
         thread, unless that work is added with an overriding priority. *)
      ; default_priority : Priority.t
      }
    [@@deriving fields, sexp_of]
  end

  module Thread = struct
    type t =
      { (* [name] is the name of the thread that the OS knows, i.e. the argument supplied
           to the most recent call to [set_thread_name] by the thread. *)
        mutable name : string
      (* [thread_id] is the OCaml thread id of the OCaml thread that this corresponds
         to.  It is an option only because we create this object before creating the
         thread.  We set it to [Some] as soon as we create the thread, and then never
         change it. *)
      ; mutable thread_id : Thread_id.t option
      (* [priority] is the priority of the thread that the OS knows, i.e. the argument
         supplied in the most recent call to [setpriority] by the thread. *)
      ; mutable priority : Priority.t
      (* A thread can be "available", meaning that it isn't working on anything, or
         doing work added to the thread pool, or serving as a helper thread.  *)
      ; mutable state :
          [ `Available | `Working | `Helper of (t[@sexp.opaque]) Helper_thread.t ]
      (* [unfinished_work] is the amount of work remaining for this thread to do.  It
         includes all the work in [work_queue], plus perhaps an additional work that is
         running. *)
      ; mutable unfinished_work : int
      (* [work_queue] is where this thread pulls work from.  Each thread has its own
         queue.  If a thread is working for the general pool, then its work queue has at
         most one element.  If a thread is a helper thread, then the work queue has all
         the unfinished work that has been added for the helper thread. *)
      ; work_queue : Work_queue.t
      }
    [@@deriving fields, sexp_of]

    let invariant t : unit =
      try
        let check invariant field = invariant (Field.get field t) in
        Fields.iter
          ~name:ignore
          ~thread_id:(check (fun o -> assert (is_some o)))
          ~priority:ignore
          ~state:ignore
          ~unfinished_work:
            (check (fun unfinished_work ->
               assert (
                 unfinished_work = Squeue.length t.work_queue
                 || unfinished_work = Squeue.length t.work_queue + 1)))
          ~work_queue:ignore
      with
      | exn -> raise_s [%message "Thread.invariant failed" (exn : exn) ~thread:(t : t)]
    ;;

    let is_available t =
      match t.state with
      | `Available -> true
      | `Working | `Helper _ -> false
    ;;

    let create priority =
      { name = ""
      ; thread_id = None
      ; priority
      ; state = `Available
      ; unfinished_work = 0
      ; work_queue = Work_queue.create ()
      }
    ;;

    let enqueue_work t work =
      t.unfinished_work <- t.unfinished_work + 1;
      Work_queue.enqueue t.work_queue (Work work)
    ;;

    let stop t = Work_queue.enqueue t.work_queue Stop

    let initialize_ocaml_thread t (cpu_affinity : Cpu_affinity.t) =
      set_thread_name t.name;
      (* We call [getpriority] to see whether we need to set the priority.  This is not a
         performance optimization.  It is done so that in programs that don't use
         priorities, we never call [setpriority], and thus prevent problems due to the
         user's "ulimit -e" being too restrictive.  The use of [getpriority] is limited to
         initialization, and is not used elsewhere in this module. *)
      if not (Priority.equal (getpriority ()) t.priority) then setpriority t.priority;
      match cpu_affinity with
      | Inherit -> ()
      | Cpuset cpuset ->
        Or_error.ok_exn
          Core.Thread.setaffinity_self_exn
          (cpuset |> Cpu_affinity.Cpuset.raw)
    ;;

    let set_name t name =
      if String.( <> ) name t.name
      then (
        set_thread_name name;
        t.name <- name)
    ;;

    let set_priority t priority =
      if not (Priority.equal t.priority priority)
      then (
        setpriority priority;
        t.priority <- priority)
    ;;
  end

  module Thread_creation_failure = struct
    type t =
      { at : Time_ns.t
      ; error : Error.t
      }
    [@@deriving fields, sexp_of]
  end

  (* [Thread_pool.t] *)
  type t =
    { id : Pool_id.t
    (** [state] starts as [`In_use] when the thread pool is created.  When the user calls
        [finished_with], it transitions to [`Finishing].  When the last work is done, it
        transitions to [`Finished] and fills [finished]. *)
    ; mutable state : [ `In_use | `Finishing | `Finished ]
    ; finished : unit Thread_safe_ivar.t
    (* [mutex] is used to protect all access to [t] and its substructures, since the
       threads actually doing the work need to access[t]. *)
    ; mutex : Mutex.t
    (** [default_priority] is the priority that will be used for work unless that work is
        added with an overriding priority.  It is set to whatever the priority is when the
        thread pool is created. *)
    ; default_priority : Priority.t
    (* [max_num_threads] is the maximum number of threads that the thread pool is allowed
       to create. *)
    ; max_num_threads : int
    (* [cpu_affinity] is the desired CPU affinity for threads in this pool. *)
    ; cpu_affinity : Cpu_affinity.t
    (* [num_threads] is the number of threads that have been created by the pool.  The
       thread pool guarantees that [num_threads <= max_num_threads]. *)
    ; mutable num_threads : int
    (* [thread_creation_failure_lockout] is the amount of time that must pass after a
       thread-creation failure before the thread pool will make another attempt to create
       a thread. *)
    ; mutable thread_creation_failure_lockout : Time_ns.Span.t
    (* [last_thread_creation_failure] has information about the last time that
       [Core.Thread.create] raised. *)
    ; mutable last_thread_creation_failure : Thread_creation_failure.t option
    (* [thread_by_id] holds all the threads that have been created by the pool. *)
    ; mutable thread_by_id : Thread.t Thread_id.Table.t
    (* [available_threads] holds all threads that have [state = `Available].  It is used
       as a stack so that the most recently used available thread is used next, on the
       theory that this is better for locality. *)
    ; mutable available_threads : Thread.t list
    (* [work_queue] holds work to be done for which no thread is available. *)
    ; work_queue : Work.t Queue.t
    (* [unfinished_work] holds the amount of work that has been submitted to the pool but
       not yet been completed. *)
    ; mutable unfinished_work : int
    ; mutable num_work_completed : int
    }
  [@@deriving fields, sexp_of]

  let invariant t : unit =
    try
      let check invariant field = invariant (Field.get field t) in
      Fields.iter
        ~id:ignore
        ~state:
          (check (function
             | `In_use | `Finishing -> ()
             | `Finished ->
               assert (t.unfinished_work = 0);
               assert (t.num_threads = 0)))
        ~finished:ignore
        ~cpu_affinity:ignore
        ~mutex:(check Mutex.invariant)
        ~default_priority:ignore
        ~max_num_threads:(check (fun max_num_threads -> assert (max_num_threads >= 1)))
        ~num_threads:
          (check (fun num_threads ->
             assert (num_threads = Hashtbl.length t.thread_by_id);
             assert (num_threads <= t.max_num_threads)))
        ~thread_creation_failure_lockout:ignore
        ~last_thread_creation_failure:ignore
        ~thread_by_id:
          (check (fun thread_by_id ->
             Thread_id.Table.invariant Thread.invariant thread_by_id))
        ~available_threads:
          (check (fun available_threads ->
             assert (List.length available_threads <= t.num_threads);
             List.iter available_threads ~f:(fun thread ->
               assert (
                 Hashtbl.exists t.thread_by_id ~f:(fun thread' ->
                   phys_equal thread thread'));
               assert (Thread.is_available thread))))
        ~work_queue:
          (check (fun work_queue ->
             (* It is possible that:

                {[
                  has_unstarted_work t
                  && t.num_threads < t.max_num_threads ]}

                This happens when adding work and [Core.Thread.create] raises.  In that
                case, the thread pool enqueues the work and continues with the threads it
                has.  If the thread pool can't make progress, then Async's thread-pool-stuck
                detection will later report it. *)
             assert (Queue.is_empty work_queue || List.is_empty t.available_threads)))
        ~unfinished_work:(check (fun unfinished_work -> assert (unfinished_work >= 0)))
        ~num_work_completed:
          (check (fun num_work_completed -> assert (num_work_completed >= 0)))
    with
    | exn ->
      raise_s [%message "Thread_pool.invariant failed" (exn : exn) ~thread_pool:(t : t)]
  ;;

  let is_finished t =
    match t.state with
    | `Finished -> true
    | `Finishing | `In_use -> false
  ;;

  let is_in_use t =
    match t.state with
    | `In_use -> true
    | `Finishing | `Finished -> false
  ;;

  let has_unstarted_work t = not (Queue.is_empty t.work_queue)

  let create ?(cpu_affinity = Cpu_affinity.Inherit) ~max_num_threads () =
    if match cpu_affinity with
      | Inherit -> false
      | Cpuset _ -> Or_error.is_error Core.Thread.setaffinity_self_exn
    then
      Or_error.error_string
        "Thread_pool.create setaffinity not supported on this platform"
    else if max_num_threads < 1
    then
      error "Thread_pool.create max_num_threads was < 1" max_num_threads [%sexp_of: int]
    else (
      let t =
        { id = Pool_id.create ()
        ; state = `In_use
        ; finished = Thread_safe_ivar.create ()
        ; mutex = Mutex.create ()
        ; default_priority = getpriority ()
        ; cpu_affinity
        ; max_num_threads
        ; num_threads = 0
        ; thread_by_id = Thread_id.Table.create ()
        ; thread_creation_failure_lockout = sec 1.
        ; last_thread_creation_failure = None
        ; available_threads = []
        ; work_queue = Queue.create ()
        ; unfinished_work = 0
        ; num_work_completed = 0
        }
      in
      Ok t)
  ;;

  let maybe_finish t =
    match t.state with
    | `In_use | `Finished -> ()
    | `Finishing ->
      if t.unfinished_work = 0
      then (
        let set x f = Option.value_exn (Field.setter f) t x in
        Fields.iter
          ~id:ignore
          ~state:(set `Finished)
          ~finished:(fun _ -> Thread_safe_ivar.fill t.finished ())
          ~mutex:ignore
          ~default_priority:ignore
          ~cpu_affinity:ignore
          ~max_num_threads:ignore
          ~num_threads:(set 0)
          ~thread_creation_failure_lockout:ignore
          ~last_thread_creation_failure:ignore
          ~thread_by_id:(fun _ ->
            Hashtbl.iter t.thread_by_id ~f:Thread.stop;
            Hashtbl.clear t.thread_by_id)
          ~available_threads:(set [])
          ~work_queue:ignore
          ~unfinished_work:ignore
          ~num_work_completed:ignore)
  ;;

  let finished_with t =
    if !debug then debug_log "Thread_pool.finished_with" t [%sexp_of: t];
    match t.state with
    | `Finishing | `Finished -> ()
    | `In_use ->
      t.state <- `Finishing;
      maybe_finish t
  ;;

  let assign_work_to_thread (thread : Thread.t) work =
    thread.state <- `Working;
    Thread.enqueue_work thread work
  ;;

  let make_thread_available t thread =
    if !debug then debug_log "make_thread_available" (thread, t) [%sexp_of: Thread.t * t];
    match Queue.dequeue t.work_queue with
    | Some work -> assign_work_to_thread thread work
    | None ->
      thread.state <- `Available;
      t.available_threads <- thread :: t.available_threads;
      maybe_finish t
  ;;

  let maybe_finish_helper_thread t (helper_thread : Thread.t Helper_thread.t) =
    match helper_thread.state with
    | `In_use | `Finished -> ()
    | `Finishing ->
      let thread = helper_thread.thread in
      if thread.unfinished_work = 0
      then (
        helper_thread.state <- `Finished;
        make_thread_available t thread)
  ;;

  let create_thread t =
    if !debug then debug_log "create_thread" t [%sexp_of: t];
    let thread = Thread.create t.default_priority in
    let ocaml_thread =
      Or_error.try_with (fun () ->
        Core.Thread.create
          ~on_uncaught_exn:`Print_to_stderr
          (fun () ->
             Thread.initialize_ocaml_thread thread t.cpu_affinity;
             let rec loop () =
               match Squeue.pop thread.work_queue with
               | Stop -> ()
               | Work work ->
                 if !debug
                 then
                   debug_log
                     "thread got work"
                     (work, thread, t)
                     [%sexp_of: Work.t * Thread.t * t];
                 Thread.set_name thread work.name;
                 Thread.set_priority thread work.priority;
                 (try work.doit () (* the actual work *) with
                  | _ -> ());
                 t.num_work_completed <- t.num_work_completed + 1;
                 if !debug
                 then
                   debug_log
                     "thread finished with work"
                     (work, thread, t)
                     [%sexp_of: Work.t * Thread.t * t];
                 Mutex.critical_section t.mutex ~f:(fun () ->
                   t.unfinished_work <- t.unfinished_work - 1;
                   thread.unfinished_work <- thread.unfinished_work - 1;
                   match thread.state with
                   | `Available ->
                     raise_s
                       [%message
                         "thread-pool thread unexpectedly available"
                           (thread : Thread.t)]
                   | `Helper helper_thread ->
                     maybe_finish_helper_thread t helper_thread
                   | `Working -> make_thread_available t thread);
                 loop ()
             in
             loop ())
          ())
    in
    Or_error.map ocaml_thread ~f:(fun ocaml_thread ->
      let thread_id = Thread_id.of_ocaml_thread ocaml_thread in
      thread.thread_id <- Some thread_id;
      t.num_threads <- t.num_threads + 1;
      Hashtbl.add_exn t.thread_by_id ~key:thread_id ~data:thread;
      thread)
  ;;

  let last_thread_creation_failure_at t =
    Option.value_map
      t.last_thread_creation_failure
      ~default:Time_ns.epoch
      ~f:Thread_creation_failure.at
  ;;

  let get_available_thread t =
    if !debug then debug_log "get_available_thread" t [%sexp_of: t];
    match t.available_threads with
    | thread :: rest ->
      t.available_threads <- rest;
      `Ok thread
    | [] ->
      if t.num_threads = t.max_num_threads
      then `None_available
      else (
        let now = Time_ns.now () in
        if Time_ns.Span.( < )
             (Time_ns.diff now (last_thread_creation_failure_at t))
             t.thread_creation_failure_lockout
        then `None_available
        else (
          match create_thread t with
          | Ok thread -> `Ok thread
          | Error error ->
            t.last_thread_creation_failure <- Some { at = now; error };
            `None_available))
  ;;

  let inc_unfinished_work t = t.unfinished_work <- t.unfinished_work + 1
  let default_thread_name = "thread-pool thread"

  let add_work ?priority ?name t doit =
    if !debug then debug_log "add_work" t [%sexp_of: t];
    if not (is_in_use t)
    then error "add_work called on finished thread pool" t [%sexp_of: t]
    else (
      let work =
        { Work.doit
        ; name = Option.value name ~default:default_thread_name
        ; priority = Option.value priority ~default:t.default_priority
        }
      in
      inc_unfinished_work t;
      (match get_available_thread t with
       | `None_available -> Queue.enqueue t.work_queue work
       | `Ok thread -> assign_work_to_thread thread work);
      Ok ())
  ;;

  let become_helper_thread_internal
        ?priority
        ?name
        t
        ~(get_thread : t -> Thread.t Or_error.t)
    =
    if !debug then debug_log "become_helper_thread_internal" t [%sexp_of: t];
    if not (is_in_use t)
    then
      error
        "become_helper_thread_internal called on finished thread pool"
        t
        [%sexp_of: t]
    else (
      match get_thread t with
      | Error _ as e -> e
      | Ok thread ->
        let helper_thread =
          { Helper_thread.default_name = Option.value name ~default:"helper_thread"
          ; default_priority = Option.value priority ~default:t.default_priority
          ; in_pool = t.id
          ; state = `In_use
          ; thread
          }
        in
        thread.state <- `Helper helper_thread;
        Ok helper_thread)
  ;;

  let create_helper_thread ?priority ?name t =
    become_helper_thread_internal ?priority ?name t ~get_thread:(fun t ->
      match get_available_thread t with
      | `Ok thread -> Ok thread
      | `None_available ->
        error ~strict:() "create_helper_thread could not get a thread" t [%sexp_of: t])
  ;;

  let become_helper_thread ?priority ?name t =
    become_helper_thread_internal ?priority ?name t ~get_thread:(fun t ->
      match Hashtbl.find t.thread_by_id (Thread_id.self ()) with
      | Some thread -> Ok thread
      | None ->
        Or_error.error_string
          "become_helper_thread not called within thread-pool thread")
  ;;

  let add_work_for_helper_thread ?priority ?name t helper_thread doit =
    if !debug
    then
      debug_log
        "add_work_for_helper_thread"
        (helper_thread, t)
        [%sexp_of: Thread.t Helper_thread.t * t];
    if not (Pool_id.equal t.id helper_thread.in_pool)
    then
      error
        "add_work_for_helper_thread called on helper thread not in pool"
        (helper_thread, t)
        [%sexp_of: Thread.t Helper_thread.t * t]
    else if not (is_in_use t)
    then
      error "add_work_for_helper_thread called on finished thread pool" t [%sexp_of: t]
    else (
      match helper_thread.state with
      | `Finishing | `Finished ->
        error
          "add_work_for_helper_thread called on helper thread no longer in use"
          (helper_thread, t)
          [%sexp_of: Thread.t Helper_thread.t * t]
      | `In_use ->
        let { Helper_thread.thread; _ } = helper_thread in
        inc_unfinished_work t;
        Thread.enqueue_work
          thread
          { Work.name =
              Option.value name ~default:(Helper_thread.default_name helper_thread)
          ; doit
          ; priority =
              Option.value
                priority
                ~default:(Helper_thread.default_priority helper_thread)
          };
        Ok ())
  ;;

  let finished_with_helper_thread t helper_thread =
    if !debug
    then
      debug_log
        "finished_with_helper_thread"
        (helper_thread, t)
        [%sexp_of: Thread.t Helper_thread.t * t];
    if not (Pool_id.equal t.id helper_thread.in_pool)
    then
      raise_s
        [%message
          "finished_with_helper_thread called on helper thread not in pool"
            (helper_thread : Thread.t Helper_thread.t)
            ~thread_pool:(t : t)]
    else (
      match helper_thread.state with
      | `Finishing | `Finished -> ()
      | `In_use ->
        helper_thread.state <- `Finishing;
        maybe_finish_helper_thread t helper_thread)
  ;;
end

(* Now we define everything to be exported, being careful to wrap everything in
   [Mutex.critical_section] that needs to be. *)
open Internal

type t = Internal.t [@@deriving sexp_of]

let thread_creation_failure_lockout = Internal.thread_creation_failure_lockout

let last_thread_creation_failure t =
  Option.map t.last_thread_creation_failure ~f:Thread_creation_failure.sexp_of_t
;;

let critical_section t ~f =
  Mutex.critical_section t.mutex ~f:(fun () ->
    protect ~f ~finally:(fun () -> if !check_invariant then invariant t))
;;

let invariant t = critical_section t ~f:(fun () -> invariant t)

let create ?cpu_affinity ~max_num_threads () =
  Result.map (create ?cpu_affinity ~max_num_threads ()) ~f:(fun t ->
    if !check_invariant then invariant t;
    t)
;;

let finished_with t = critical_section t ~f:(fun () -> finished_with t)

(* We do not use [critical_section] for [block_until_finished] because it is already
   thread safe, and we do not want it to hold [t]'s lock while blocking, because we must
   allow the finishing threads to acquire [t]'s lock. *)
let block_until_finished t = Thread_safe_ivar.read t.finished
let cpu_affinity = cpu_affinity
let default_priority = default_priority
let has_unstarted_work = has_unstarted_work
let max_num_threads = max_num_threads
let num_threads = num_threads
let num_work_completed = num_work_completed
let unfinished_work = unfinished_work

module Helper_thread = struct
  open Helper_thread

  type t = Thread.t Helper_thread.t [@@deriving sexp_of]

  let default_name = default_name
  let default_priority = default_priority
end

let add_work ?priority ?name t doit =
  critical_section t ~f:(fun () -> add_work ?priority ?name t doit)
;;

let become_helper_thread ?priority ?name t =
  critical_section t ~f:(fun () -> become_helper_thread ?priority ?name t)
;;

let create_helper_thread ?priority ?name t =
  critical_section t ~f:(fun () -> create_helper_thread ?priority ?name t)
;;

let add_work_for_helper_thread ?priority ?name t helper_thread doit =
  critical_section t ~f:(fun () ->
    add_work_for_helper_thread ?priority ?name t helper_thread doit)
;;

let finished_with_helper_thread t helper_thread =
  critical_section t ~f:(fun () -> finished_with_helper_thread t helper_thread)
;;

module Private = struct
  let check_invariant = check_invariant
  let default_thread_name = default_thread_name
  let is_finished = is_finished
  let is_in_use = is_in_use

  let set_last_thread_creation_failure t at =
    t.last_thread_creation_failure <- Some { at; error = Error.of_string "fake-error" }
  ;;

  let set_thread_creation_failure_lockout t span =
    t.thread_creation_failure_lockout <- span
  ;;
end
