open! Core_kernel
open! Import
include Types.Scheduler

let events t = t.time_source.events

let set_execution_context t execution_context =
  (* Avoid a caml_modify in most cases. *)
  if not (phys_equal t.current_execution_context execution_context)
  then t.current_execution_context <- execution_context
;;
