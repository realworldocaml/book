open! Core
open Poly
open! Async
open! Thread_safe_pipe

let%expect_test "Thread_safe_pipe" =
  let module P = Thread_safe_pipe in
  let r, p = P.create () in
  assert (
    try
      P.write p 13 ~if_closed:Raise;
      false
    with
    | _ -> true);
  let throttle = Throttle.create ~continue_on_error:false ~max_concurrent_jobs:1 in
  let run_in_thread f =
    don't_wait_for (Throttle.enqueue throttle (fun () -> In_thread.run f))
  in
  let num_elts = 100 in
  for i = 0 to num_elts - 1 do
    run_in_thread (fun () -> P.write p i ~if_closed:Raise)
  done;
  run_in_thread (fun () -> P.close p);
  Pipe.to_list r >>| fun list -> assert (list = List.init num_elts ~f:Fn.id)
;;

let%expect_test "Thread_safe_pipe2" =
  In_thread.run (fun () -> Thread_safe_pipe.create ())
  >>= fun (pipe_reader, pipe_writer) ->
  assert (
    (* [write] raises if we're in Async. *)
    try
      Thread_safe_pipe.write pipe_writer 13 ~if_closed:Raise;
      false
    with
    | _ -> true);
  let throttle = Throttle.create ~continue_on_error:false ~max_concurrent_jobs:1 in
  let run_in_thread f =
    don't_wait_for (Throttle.enqueue throttle (fun () -> In_thread.run f))
  in
  let num_elts = 100 in
  for i = 0 to num_elts - 1 do
    run_in_thread (fun () ->
      Thread_safe_pipe.write_without_pushback pipe_writer i ~if_closed:Raise)
  done;
  run_in_thread (fun () -> Thread_safe_pipe.close pipe_writer);
  Pipe.to_list pipe_reader >>| fun list -> assert (list = List.init num_elts ~f:Fn.id)
;;
