open! Core
open! Import
include Scheduler

let enqueue_job execution_context f a = enqueue (t ()) execution_context f a

let thread_safe_enqueue_job execution_context f a =
  thread_safe_enqueue_external_job (t ()) execution_context f a
;;

let current_execution_context () = current_execution_context (t ())
let cycle_count () = cycle_count (t ())
let cycle_start_ns () = cycle_start (t ())
let cycle_start () = Time_ns.to_time_float_round_nearest (cycle_start_ns ())
let cycle_times_ns () = map_cycle_times (t ()) ~f:Fn.id
let cycle_times () = map_cycle_times (t ()) ~f:Time_ns.Span.to_span_float_round_nearest
let total_cycle_time () = total_cycle_time (t ())
let last_cycle_time () = last_cycle_time (t ())
let long_cycles ~at_least = long_cycles (t ()) ~at_least
let event_precision_ns () = event_precision (t ())
let event_precision () = Time_ns.Span.to_span_float_round_nearest (event_precision_ns ())

let set_max_num_jobs_per_priority_per_cycle i =
  set_max_num_jobs_per_priority_per_cycle (t ()) i
;;

let max_num_jobs_per_priority_per_cycle () = max_num_jobs_per_priority_per_cycle (t ())
let set_record_backtraces bool = set_record_backtraces (t ()) bool
let force_current_cycle_to_end () = force_current_cycle_to_end (t ())
let yield () = yield (t ())

let yield_until_no_jobs_remain ?may_return_immediately () =
  yield_until_no_jobs_remain ?may_return_immediately (t ())
;;

let yield_every ~n =
  let yield_every = Staged.unstage (yield_every ~n) in
  let t = t () in
  stage (fun () -> yield_every t)
;;

let num_jobs_run () = num_jobs_run (t ())
let num_pending_jobs () = num_pending_jobs (t ())

module Expert = struct
  let run_cycles_until_no_jobs_remain = run_cycles_until_no_jobs_remain
  let last_cycle_num_jobs () = last_cycle_num_jobs (t ())
  let run_every_cycle_start f = run_every_cycle_start (t ()) ~f
  let run_every_cycle_end f = run_every_cycle_end (t ()) ~f
  let add_every_cycle_start_hook ~f = add_every_cycle_start_hook (t ()) ~f
  let add_every_cycle_end_hook ~f = add_every_cycle_end_hook (t ()) ~f

  let remove_every_cycle_start_hook_exn handle =
    remove_every_cycle_start_hook_exn (t ()) handle
  ;;

  let remove_every_cycle_end_hook_exn handle =
    remove_every_cycle_end_hook_exn (t ()) handle
  ;;

  let with_execution_context execution_context f =
    with_execution_context (t ()) execution_context ~f
  ;;

  let with_execution_context1 execution_context ~f x =
    with_execution_context1 (t ()) execution_context ~f x
  ;;
end

module Private = Scheduler
