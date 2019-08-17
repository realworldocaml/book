(* Unit tests are in ../../lib_test/thread_safe_test.ml. *)

open! Core
open! Async_kernel
open! Async_unix
open! Import

type 'a t = 'a Pipe.Writer.t [@@deriving sexp_of]

let in_async ?wakeup_scheduler f = Thread_safe.run_in_async_exn ?wakeup_scheduler f
let in_async_wait f = Thread_safe.run_in_async_wait_exn f

module Written_or_closed = struct
  type t =
    | Written
    | Closed
end

module If_closed = struct
  type 'a t =
    | Raise : unit t
    | Return : Written_or_closed.t t

  let closed : type a. a t -> a = function
    | Raise -> raise_s [%message "pipe is closed"]
    | Return -> Closed
  ;;

  let written : type a. a t -> a = function
    | Raise -> ()
    | Return -> Written
  ;;
end

let in_async_unless_closed ?wakeup_scheduler t f ~if_closed =
  in_async ?wakeup_scheduler (fun () ->
    if Pipe.is_closed t
    then If_closed.closed if_closed
    else (
      f ();
      If_closed.written if_closed))
;;

let in_async_unless_closed_wait t f ~if_closed =
  in_async_wait (fun () ->
    if Pipe.is_closed t
    then return (If_closed.closed if_closed)
    else (
      let%map () = f () in
      If_closed.written if_closed))
;;

let create () =
  if Thread_safe.am_holding_async_lock () then Pipe.create () else in_async Pipe.create
;;

let pushback t = in_async_wait (fun () -> Pipe.pushback t)

let transfer_in t ~from ~if_closed =
  in_async_unless_closed_wait t ~if_closed (fun () -> Pipe.transfer_in t ~from)
;;

let write t a ~if_closed =
  in_async_unless_closed_wait t ~if_closed (fun () -> Pipe.write t a)
;;

let transfer_in_without_pushback ?wakeup_scheduler t ~from ~if_closed =
  in_async_unless_closed ?wakeup_scheduler t ~if_closed (fun () ->
    Pipe.transfer_in_without_pushback t ~from)
;;

let write_without_pushback ?wakeup_scheduler t a ~if_closed =
  in_async_unless_closed ?wakeup_scheduler t ~if_closed (fun () ->
    Pipe.write_without_pushback t a)
;;

let close t = in_async (fun () -> Pipe.close t)
let is_closed t = in_async (fun () -> Pipe.is_closed t)
let closed t = in_async_wait (fun () -> Pipe.closed t)
