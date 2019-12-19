open Core_kernel
open Import
open Deferred_std
module Deferred = Deferred1
module Scheduler = Scheduler1
module Stream = Tail.Stream
module Monitor = Monitor0
include Monitor

type monitor = t [@@deriving sexp_of]

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~name:ignore
      ~here:ignore
      ~id:ignore
      ~parent:ignore
      ~next_error:(check (fun next_error -> assert (Ivar.is_empty next_error)))
      ~handlers_for_all_errors:ignore
      ~tails_for_all_errors:ignore
      ~has_seen_error:ignore
      ~is_detached:ignore)
;;

let current_execution_context () = Scheduler.(current_execution_context (t ()))
let current () = Execution_context.monitor (current_execution_context ())

let depth t =
  let rec loop t n =
    match t.parent with
    | None -> n
    | Some t -> loop t (n + 1)
  in
  loop t 0
;;

type 'a with_optional_monitor_name =
  ?here:Source_code_position.t -> ?info:Info.t -> ?name:string -> 'a

let detach t = t.is_detached <- true

type handler_state =
  | Uninitialized
  | Running of (Execution_context.t * (exn -> unit)) Bag.Elt.t
  | Terminated

let detach_and_iter_errors t ~f =
  detach t;
  let scheduler = Scheduler.t () in
  let execution_context = Scheduler.current_execution_context scheduler in
  let handler_state_ref = ref Uninitialized in
  let run_f exn =
    match !handler_state_ref with
    | Uninitialized -> assert false
    | Terminated -> ()
    | Running bag_elt ->
      (try f exn with
       | inner_exn ->
         handler_state_ref := Terminated;
         Bag.remove t.handlers_for_all_errors bag_elt;
         (* [run_f] always runs in [execution_context].  Hence, [raise inner_exn] sends
            [inner_exn] to [execution_context]'s monitor, i.e. the monitor in effect when
            [detach_and_iter_errors] was called. *)
         raise inner_exn)
  in
  handler_state_ref
  := Running (Bag.add t.handlers_for_all_errors (execution_context, run_f))
;;

let detach_and_get_error_stream t =
  detach t;
  let tail = Tail.create () in
  t.tails_for_all_errors <- tail :: t.tails_for_all_errors;
  Tail.collect tail
;;

let get_next_error t = Ivar.read t.next_error

let detach_and_get_next_error t =
  detach t;
  get_next_error t
;;

let create ?here ?info ?name () =
  let parent = current () in
  create_with_parent ?here ?info ?name (Some parent)
;;

module Exn_for_monitor = struct
  type t =
    { exn : exn
    ; backtrace : Backtrace.t option
    ; backtrace_history : Backtrace.t list
    ; monitor : Monitor.t
    }

  let backtrace_truncation_heuristics =
    let job_queue = "Called from file \"job_queue.ml\"" in
    let deferred0 = "Called from file \"deferred0.ml\"" in
    let deferred1 = "Called from file \"deferred1.ml\"" in
    let monitor = "Called from file \"monitor.ml\"" in
    let import0 = "Raised at file \"import0.ml\"" in
    let error = "Called from file \"error.ml\"" in
    fun traces ->
      (* ../test/test_try_with_error_display.ml makes sure this stays up-to-date. *)
      let traces =
        match traces with
        | t1 :: rest when String.is_prefix t1 ~prefix:import0 ->
          (match rest with
           | t2 :: rest when String.is_prefix t2 ~prefix:error ->
             (match rest with
              | t3 :: rest when String.is_prefix t3 ~prefix:error -> rest
              | _ -> rest)
           | _ -> rest)
        | _ -> traces
      in
      match List.rev traces with
      | t1 :: rest when String.is_prefix t1 ~prefix:job_queue ->
        (match rest with
         | t2 :: rest when String.is_prefix t2 ~prefix:job_queue ->
           (match rest with
            | t2 :: rest
              when String.is_prefix t2 ~prefix:deferred0
                (* bind *)
                || String.is_prefix t2 ~prefix:deferred1
                (* map *)
                || String.is_prefix t2 ~prefix:monitor
              (* try_with *) -> List.rev rest
            | _ -> List.rev rest)
         | _ -> List.rev rest)
      | _ -> traces
  ;;

  let sexp_of_t { exn; backtrace; backtrace_history; monitor } =
    let monitor =
      let name =
        match Info.to_string_hum monitor.name with
        | "" -> None
        | s -> Some s
      in
      let pos =
        match monitor.here with
        | None -> None
        | Some here ->
          (* We display the full filename, whereas backtraces only have basenames, but
             perhaps that's what should change. *)
          let column = here.pos_cnum - here.pos_bol in
          Some
            (sprintf
               "file %S, line %d, characters %d-%d"
               here.pos_fname
               here.pos_lnum
               column
               column)
      in
      match pos, name with
      | None, None -> []
      | Some pos, None -> [ sprintf "Caught by monitor at %s" pos ]
      | None, Some name -> [ sprintf "Caught by monitor %s" name ]
      | Some pos, Some name -> [ sprintf "Caught by monitor %s at %s" name pos ]
    in
    let backtrace =
      let backtrace =
        match backtrace with
        | None -> []
        | Some backtrace -> Backtrace.to_string_list backtrace
      in
      backtrace_truncation_heuristics backtrace @ monitor
    in
    let list_if_not_empty = function
      | [] -> None
      | _ :: _ as l -> Some l
    in
    [%sexp
      (exn : exn)
    , (list_if_not_empty backtrace : (string list option[@sexp.option]))
    , `backtrace_history
        (list_if_not_empty backtrace_history : (Backtrace.t list option[@sexp.option]))]
  ;;
end

exception Error_ of Exn_for_monitor.t

let () =
  Sexplib.Conv.Exn_converter.add [%extension_constructor Error_] (function
    | Error_ t -> [%sexp "monitor.ml.Error" :: (t : Exn_for_monitor.t)]
    | _ ->
      (* Reaching this branch indicates a bug in sexplib. *)
      assert false)
;;

let extract_exn exn =
  match exn with
  | Error_ error -> error.exn
  | exn -> exn
;;

let send_exn t ?backtrace exn =
  let exn =
    match exn with
    | Error_ _ -> exn
    | _ ->
      let backtrace =
        match backtrace with
        | None -> None
        | Some `Get -> Some (Backtrace.Exn.most_recent ())
        | Some (`This b) -> Some b
      in
      let backtrace_history = (current_execution_context ()).backtrace_history in
      Error_ { Exn_for_monitor.exn; backtrace; backtrace_history; monitor = t }
  in
  if Debug.monitor_send_exn
  then Debug.log "Monitor.send_exn" (t, exn) [%sexp_of: t * exn];
  t.has_seen_error <- true;
  let scheduler = Scheduler.t () in
  let rec loop t =
    Ivar.fill t.next_error exn;
    t.next_error <- Ivar.create ();
    if t.is_detached
    then (
      if Debug.monitor_send_exn
      then
        Debug.log "Monitor.send_exn found listening monitor" (t, exn) [%sexp_of: t * exn];
      Bag.iter t.handlers_for_all_errors ~f:(fun (execution_context, f) ->
        Scheduler.enqueue scheduler execution_context f exn);
      List.iter t.tails_for_all_errors ~f:(fun tail -> Tail.extend tail exn))
    else (
      match t.parent with
      | Some t' -> loop t'
      | None ->
        (* Do not change this branch to print the exception or to exit.  Having the
           scheduler raise an uncaught exception is the necessary behavior for programs
           that call [Scheduler.go] and want to handle it. *)
        Scheduler.(got_uncaught_exn (t ())) exn (!Async_kernel_config.task_id ()))
  in
  loop t
;;

module Exported_for_scheduler = struct
  let within_context context f =
    Scheduler.(with_execution_context (t ())) context ~f:(fun () ->
      match Result.try_with f with
      | Ok x -> Ok x
      | Error exn ->
        send_exn (Execution_context.monitor context) exn ~backtrace:`Get;
        Error ())
  ;;

  type 'a with_options = ?monitor:t -> ?priority:Priority.t -> 'a

  let within_gen ?monitor ?priority f =
    let tmp_context =
      Execution_context.create_like (current_execution_context ()) ?monitor ?priority
    in
    within_context tmp_context f
  ;;

  let within' ?monitor ?priority f =
    match within_gen ?monitor ?priority f with
    | Error () -> Deferred.never ()
    | Ok d -> d
  ;;

  let within_v ?monitor ?priority f =
    match within_gen ?monitor ?priority f with
    | Error () -> None
    | Ok x -> Some x
  ;;

  let within ?monitor ?priority f =
    match within_gen ?monitor ?priority f with
    | Error () -> ()
    | Ok () -> ()
  ;;

  let schedule_with_data ?monitor ?priority work x =
    let scheduler = Scheduler.t () in
    Scheduler.enqueue
      scheduler
      (Execution_context.create_like
         (Scheduler.current_execution_context scheduler)
         ?monitor
         ?priority)
      work
      x
  ;;

  let schedule ?monitor ?priority work = schedule_with_data ?monitor ?priority work ()

  let schedule' =
    (* For performance, we use [schedule_with_data] with a closed function, and inline
       [Deferred.create]. *)
    let upon_work_fill_i (work, i) = upon (work ()) (fun a -> Ivar.fill i a) in
    fun ?monitor ?priority work ->
      let i = Ivar.create () in
      schedule_with_data ?monitor ?priority upon_work_fill_i (work, i);
      Ivar.read i
  ;;

  let preserve_execution_context f =
    let scheduler = Scheduler.t () in
    let execution_context = Scheduler.current_execution_context scheduler in
    stage (fun a -> Scheduler.enqueue scheduler execution_context f a)
  ;;

  let preserve_execution_context' f =
    let scheduler = Scheduler.t () in
    let execution_context = Scheduler.current_execution_context scheduler in
    let call_and_fill (f, a, i) = upon (f a) (fun r -> Ivar.fill i r) in
    stage (fun a ->
      Deferred.create (fun i ->
        Scheduler.enqueue scheduler execution_context call_and_fill (f, a, i)))
  ;;
end

open Exported_for_scheduler

let stream_iter stream ~f =
  let rec loop stream =
    Stream.next stream
    >>> function
    | Nil -> ()
    | Cons (v, stream) ->
      loop stream;
      f v
  in
  loop stream
;;

(* An ['a Ok_and_exns.t] represents the output of a computation running in a detached
   monitor. *)
module Ok_and_exns = struct
  type 'a t =
    { ok : 'a Deferred.t
    ; exns : exn Stream.t
    }
  [@@deriving fields, sexp_of]

  let create ?here ?info ?name ~run f =
    (* We call [create_with_parent None] because [monitor] does not need a parent.  It
       does not because we call [detach_and_get_error_stream monitor] and deal with the
       errors explicitly, thus [send_exn] would never propagate an exn past [monitor]. *)
    let monitor = create_with_parent ?here ?info ?name None in
    let exns = detach_and_get_error_stream monitor in
    let ok =
      match run with
      | `Now -> within' ~monitor f
      | `Schedule -> schedule' ~monitor f
    in
    { ok; exns }
  ;;
end

let fill_result_and_handle_background_errors
      result_filler
      result
      exns
      handle_exns_after_result
  =
  if Ivar_filler.is_empty result_filler
  then (
    Ivar_filler.fill result_filler result;
    handle_exns_after_result exns)
;;

module Expert = struct
  let try_with_log_exn : (exn -> unit) ref =
    ref (fun exn ->
      raise_s
        [%message "failed to set [Monitor.Expert.try_with_log_exn]" (exn : Exn.t)])
  ;;
end

let make_handle_exn rest =
  match rest with
  | `Log ->
    (* We are careful to not close over current context, which is not needed. *)
    !Expert.try_with_log_exn
  | `Raise ->
    let parent = current () in
    fun exn -> send_exn parent exn ?backtrace:None
  | `Call f ->
    let parent = current () in
    fun exn -> within ~monitor:parent (fun () -> f exn)
;;

let try_with
      ?here
      ?info
      ?(name = "")
      ?extract_exn:(do_extract_exn = false)
      ?(run = `Schedule)
      ?(rest = `Log)
      f
  =
  let { Ok_and_exns.ok; exns } = Ok_and_exns.create ?here ?info ~name ~run f in
  let handle_exn = make_handle_exn rest in
  let handle_exns_after_result exns = stream_iter exns ~f:handle_exn in
  (* We run [within' ~monitor:main] to avoid holding on to references to the evaluation
     context in which [try_with] was called.  This avoids a space leak when a chain of
     [try_with]'s are run each nested within the previous one.  Without the [within'], the
     error handling for the innermost [try_with] would keep alive the entire chain. *)
  within' ~monitor:main (fun () ->
    if Deferred.is_determined ok
    then (
      handle_exns_after_result exns;
      return (Ok (Deferred.value_exn ok)))
    else (
      let result_filler, result = Ivar_filler.create () in
      upon ok (fun res ->
        fill_result_and_handle_background_errors
          result_filler
          (Ok res)
          exns
          handle_exns_after_result);
      upon (Stream.next exns) (function
        | Nil -> assert false
        | Cons (exn, exns) ->
          let exn = if do_extract_exn then extract_exn exn else exn in
          fill_result_and_handle_background_errors
            result_filler
            (Error exn)
            exns
            handle_exns_after_result);
      result))
;;

let try_with_or_error ?here ?info ?(name = "try_with_or_error") ?extract_exn f =
  try_with f ?here ?info ~name ?extract_exn ~run:`Now ~rest:`Log
  >>| Or_error.of_exn_result
;;

let try_with_join_or_error ?here ?info ?(name = "try_with_join_or_error") ?extract_exn f =
  try_with_or_error f ?here ?info ~name ?extract_exn >>| Or_error.join
;;

let protect ?here ?info ?(name = "Monitor.protect") ?extract_exn ?run f ~finally =
  let%bind r = try_with ?extract_exn ?here ?info ?run ~name f in
  let%map fr = try_with ~extract_exn:false ?here ?info ~name:"finally" finally in
  match r, fr with
  | Error exn, Error finally_exn ->
    raise_s [%message "Async finally" (exn : exn) (finally_exn : exn)]
  | Error e, Ok () | Ok _, Error e -> raise e
  | Ok r, Ok () -> r
;;

let handle_errors ?here ?info ?name f handler =
  let { Ok_and_exns.ok; exns } = Ok_and_exns.create ?here ?info ?name ~run:`Now f in
  stream_iter exns ~f:handler;
  ok
;;

let catch_stream ?here ?info ?name f =
  let { Ok_and_exns.exns; _ } =
    Ok_and_exns.create ?here ?info ?name ~run:`Now (fun () ->
      f ();
      return ())
  in
  exns
;;

let catch ?here ?info ?name f =
  match%map Stream.next (catch_stream ?here ?info ?name f) with
  | Cons (x, _) -> x
  | Nil -> raise_s [%message "Monitor.catch got unexpected empty stream"]
;;

let catch_error ?here ?info ?name f = catch ?here ?info ?name f >>| Error.of_exn
