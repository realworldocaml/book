open! Core_kernel
open! Thread_safe_queue

(* The benchmarks below first allocate the queue and a queue elt and then do a
   [Gc.full_major] so that the queue and elt are in the major heap, which should more
   accurately reflect common usage than if they were in the minor heap. *)

let%bench_fun "enqueue + dequeue_exn of immediate" =
  let t = create () in
  enqueue t ();
  ignore (dequeue_exn t : unit);
  Gc.full_major ();
  fun () ->
    enqueue t ();
    ignore (dequeue_exn t : unit)
;;

let%bench_fun "enqueue + dequeue_exn of young object" =
  let t = create () in
  enqueue t (ref ());
  ignore (dequeue_exn t : unit ref);
  Gc.full_major ();
  fun () ->
    enqueue t (ref ());
    ignore (dequeue_exn t : unit ref)
;;

let%bench_fun "enqueue + dequeue_exn of old object" =
  let r = ref () in
  let t = create () in
  enqueue t r;
  ignore (dequeue_exn t : unit ref);
  Gc.full_major ();
  fun () ->
    enqueue t r;
    ignore (dequeue_exn t : unit ref)
;;
