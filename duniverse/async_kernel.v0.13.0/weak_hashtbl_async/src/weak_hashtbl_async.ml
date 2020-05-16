open! Core_kernel
open! Async_kernel
open! Import
include Weak_hashtbl

let create ?growth_allowed ?size hashable =
  let t = create ?growth_allowed ?size hashable in
  (* To avoid having keys around that should be cleaned, we must ensure that after any
     call to [thread_safe_f], there is a call to
     [reclaim_space_for_keys_with_unused_data].  We do this via [reclaim_will_happen],
     which, if [true], guarantees that a call to [reclaim_space_for_keys_with_unused_data]
     will happen in the future.  It is OK if we have multiple reclaims extant
     simultaneously, since they are async jobs. *)
  let reclaim_will_happen = ref false in
  let reclaim () =
    reclaim_will_happen := false;
    reclaim_space_for_keys_with_unused_data t
  in
  set_run_when_unused_data t ~thread_safe_f:(fun () ->
    if not !reclaim_will_happen
    then (
      reclaim_will_happen := true;
      Async_kernel_scheduler.thread_safe_enqueue_job Execution_context.main reclaim ()));
  t
;;

let reclaim_space_for_keys_with_unused_data `Do_not_use = assert false
let set_run_when_unused_data `Do_not_use = assert false
