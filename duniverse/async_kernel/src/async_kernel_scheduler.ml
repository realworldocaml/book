open! Core_kernel
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
let yield_until_no_jobs_remain () = yield_until_no_jobs_remain (t ())

let yield_every ~n =
  let yield_every = Staged.unstage (yield_every ~n) in
  let t = t () in
  stage (fun () -> yield_every t)
;;

let num_jobs_run () = num_jobs_run (t ())
let num_pending_jobs () = num_pending_jobs (t ())

module Expert = struct
  let run_cycles_until_no_jobs_remain = run_cycles_until_no_jobs_remain
  let set_on_start_of_cycle f = set_on_start_of_cycle (t ()) f
  let set_on_end_of_cycle f = set_on_end_of_cycle (t ()) f
end

module Private = Scheduler
