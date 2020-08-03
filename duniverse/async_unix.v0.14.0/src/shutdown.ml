(* Unit tests are in ../../lib_test/shutdown_tests.ml *)

open Core
open Import

let debug = Debug.shutdown
let todo = ref []

let at_shutdown f =
  let backtrace = Backtrace.get () in
  if debug then Debug.log "at_shutdown" backtrace [%sexp_of: Backtrace.t];
  todo := (backtrace, f) :: !todo
;;

let shutting_down_ref = ref `No
let default_force_ref = ref (fun () -> Clock.after (sec 10.))
let default_force () = !default_force_ref
let set_default_force force = default_force_ref := force
let shutting_down () = !shutting_down_ref

let is_shutting_down () =
  match shutting_down () with
  | `No -> false
  | `Yes _ -> true
;;

(* Be careful to ensure [shutdown] doesn't raise just because
   stderr is closed *)
let ignore_exn f =
  try f () with
  | _ -> ()
;;

let exit_reliably status =
  match (exit status : Nothing.t) with
  | exception exn ->
    ignore_exn (fun () -> Core.Debug.eprints "Caml.exit raised" exn [%sexp_of: Exn.t]);
    Core.Unix.exit_immediately (if status = 0 then 1 else status)
  | _ -> .
;;

let shutdown ?force status =
  if debug then ignore_exn (fun () -> Debug.log "shutdown" status [%sexp_of: int]);
  match !shutting_down_ref with
  | `Yes status' ->
    if status <> 0 && status' <> 0 && status <> status'
    then
      raise_s
        [%message "shutdown with inconsistent status" (status : int) (status' : int)]
    else if status' = 0 && status <> 0
    then shutting_down_ref := `Yes status
  | `No ->
    shutting_down_ref := `Yes status;
    upon
      (Deferred.all
         (List.map !todo ~f:(fun (backtrace, f) ->
            let%map result = Monitor.try_with_or_error f in
            (match result with
             | Ok () -> ()
             | Error error ->
               ignore_exn (fun () ->
                 Core.Debug.eprints
                   "at_shutdown function raised"
                   (error, backtrace)
                   [%sexp_of: Error.t * Backtrace.t]));
            if debug
            then
              ignore_exn (fun () ->
                Debug.log
                  "one at_shutdown function finished"
                  backtrace
                  [%sexp_of: Backtrace.t]);
            result)))
      (fun results ->
         match shutting_down () with
         | `No -> assert false
         | `Yes status ->
           let status =
             match Or_error.combine_errors_unit results with
             | Ok () -> status
             | Error _ -> if status = 0 then 1 else status
           in
           exit_reliably status);
    let force =
      match force with
      | None -> !default_force_ref ()
      | Some f -> f
    in
    upon force (fun () ->
      ignore_exn (fun () -> Debug.log_string "Shutdown forced.");
      exit_reliably 1)
;;

let shutdown_on_unhandled_exn () =
  Monitor.detach_and_iter_errors Monitor.main ~f:(fun exn ->
    ignore_exn (fun () ->
      Debug.log "shutting down due to unhandled exception" exn [%sexp_of: exn]);
    try shutdown 1 with
    | _ ->
      (* The above [shutdown] call raises if we have already called shutdown with a
         different non-zero status. *)
      ())
;;

let exit ?force status =
  shutdown ?force status;
  Deferred.never ()
;;

let don't_finish_before =
  let proceed_with_shutdown = Ivar.create () in
  let num_waiting = ref 0 in
  let check () = if !num_waiting = 0 then Ivar.fill proceed_with_shutdown () in
  at_shutdown (fun () ->
    check ();
    Ivar.read proceed_with_shutdown);
  fun d ->
    match shutting_down () with
    | `Yes _ ->
      ()
    | `No ->
      incr num_waiting;
      upon d (fun () ->
        decr num_waiting;
        match shutting_down () with
        | `No -> ()
        | `Yes _ -> check ())
;;
