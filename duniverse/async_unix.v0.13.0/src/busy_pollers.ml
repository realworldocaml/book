open Core
open Import

module Poller = struct
  module U = struct
    type 'a t =
      { execution_context : Execution_context.t
      ; result : 'a Ivar.t
      ; poll : unit -> [ `Stop_polling of 'a | `Continue_polling ]
      ; mutable is_alive : bool
      }
    [@@deriving fields, sexp_of]

    let invariant a_invariant t : unit =
      Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
        let check f = Invariant.check_field t f in
        Fields.iter
          ~execution_context:(check Execution_context.invariant)
          ~result:(check (Ivar.invariant a_invariant))
          ~poll:ignore
          ~is_alive:ignore)
    ;;
  end

  type t = T : _ U.t -> t

  let sexp_of_t (T u) = [%sexp_of: _ U.t] u
  let invariant (T u) = U.invariant ignore u
  let is_alive (T u) = U.is_alive u
end

type t =
  { (* [kernel_scheduler] is [@sexp.opaque] so that one doesn't get two copies of the
       kernel scheduler in sexps of the scheduler, which already has its own
       [kernel_scheduler] field. *)
    kernel_scheduler : (Kernel_scheduler.t[@sexp.opaque])
  ;
    mutable pollers : Poller.t array
  }
[@@deriving fields, sexp_of]

let is_empty t = Array.is_empty t.pollers
let create () = { kernel_scheduler = Kernel_scheduler.t (); pollers = [||] }

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~kernel_scheduler:(check Kernel_scheduler.invariant)
      ~pollers:
        (check (fun pollers ->
           Array.iter pollers ~f:(fun poller ->
             Poller.invariant poller;
             assert (Poller.is_alive poller)))))
;;

let poll t =
  let kernel_scheduler = t.kernel_scheduler in
  let pollers = t.pollers in
  let killed_some = ref false in
  for i = 0 to Array.length pollers - 1 do
    let (Poller.T u) = pollers.(i) in
    Kernel_scheduler.set_execution_context kernel_scheduler u.execution_context;
    let should_kill =
      try
        match u.poll () with
        | `Continue_polling -> false
        | `Stop_polling a ->
          Ivar.fill u.result a;
          true
      with
      | exn ->
        Monitor.send_exn
          (Execution_context.monitor u.execution_context)
          exn
          ~backtrace:`Get;
        true
    in
    if should_kill
    then (
      u.is_alive <- false;
      killed_some := true)
  done;
  if !killed_some then t.pollers <- Array.filter t.pollers ~f:Poller.is_alive
;;

let add t poll =
  let execution_context =
    Kernel_scheduler.current_execution_context t.kernel_scheduler
  in
  let result = Ivar.create () in
  let poller = Poller.(T { execution_context; result; poll; is_alive = true }) in
  let n = Array.length t.pollers in
  t.pollers <- Array.init (1 + n) ~f:(fun i -> if i < n then t.pollers.(i) else poller);
  Ivar.read result
;;
