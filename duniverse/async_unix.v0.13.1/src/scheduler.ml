(* [Raw_scheduler] is distinct from [Scheduler], because the former exposes some things
   that are used internally within Async that are not exposed in scheduler.mli.
   Also, it breaks a cyclic dependency [Raw_scheduler -> Log -> Scheduler]. *)

open! Core
open! Import
include Raw_scheduler

let current_execution_context = Async_kernel_scheduler.current_execution_context

let time_spent_waiting_for_io () =
  let t = t () in
  t.time_spent_waiting_for_io
  |> Tsc.Span.to_ns ~calibrator:(force Time_stamp_counter.calibrator)
  |> Time_ns.Span.of_int63_ns
;;

let set_min_inter_cycle_timeout min_inter_cycle_timeout =
  let t = t () in
  if Time_ns.Span.( > )
       min_inter_cycle_timeout
       (t.max_inter_cycle_timeout :> Time_ns.Span.t)
  then
    Error.raise
      ([%message
        "min_inter_cycle_timeout too large"
          (min_inter_cycle_timeout : Time_ns.Span.t)
          (t.max_inter_cycle_timeout : Max_inter_cycle_timeout.t)]
       |> [%of_sexp: Error.t]);
  t.min_inter_cycle_timeout <- Min_inter_cycle_timeout.create_exn min_inter_cycle_timeout
;;

let max_num_open_file_descrs () = max_num_open_file_descrs (t ())
let max_num_threads () = max_num_threads (t ())
let _ = current_execution_context
