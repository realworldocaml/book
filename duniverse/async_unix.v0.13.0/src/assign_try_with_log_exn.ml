open! Core
open! Async_kernel
open! Import

(* We want to get a log of errors sent to [Monitor.try_with] after the initial return, so
   on initialization we redirect them to [Log.Global.error].  However, logging errors
   isn't cheap and there are issues with thread fairness when outputting to stderr (which
   is the default in many cases for [Global.error]), so, to prevent the [Log] [Writer.t]
   buffer from growing without bound, we limit the number of currently unflushed error
   messages created by [try_with_log_exn]. *)
let try_with_log_exn =
  let max_unflushed_errors = 10 in
  let current_unflushed_errors = ref 0 in
  let log sexp = Log.Global.sexp sexp ~level:`Error in
  fun exn ->
    if !current_unflushed_errors < max_unflushed_errors
    then (
      incr current_unflushed_errors;
      log
        [%message
          "Exception raised to [Monitor.try_with] that already returned."
            "This error was captured by a default handler in [Async.Log]."
            (exn : exn)];
      if !current_unflushed_errors = max_unflushed_errors
      then
        log
          [%message
            "Stopped logging exceptions raised to [Monitor.try_with] that already \
             returned until error log can be flushed."];
      upon (Log.Global.flushed ()) (fun () -> decr current_unflushed_errors))
;;

let () = Async_kernel.Monitor.Expert.try_with_log_exn := try_with_log_exn
