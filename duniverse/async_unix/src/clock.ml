open! Core
open! Import
open Async_kernel.Clock_ns

let run_at time f a = run_at (Time_ns.of_time_float_round_nearest time) f a
let run_after span f a = run_after (Time_ns.Span.of_span_float_round_nearest span) f a
let at time = at (Time_ns.of_time_float_round_nearest time)
let after span = after (Time_ns.Span.of_span_float_round_nearest span)
let with_timeout span d = with_timeout (Time_ns.Span.of_span_float_round_nearest span) d

module Event = struct
  module Abort_result = Event.Abort_result
  module Fired = Event.Fired
  module Reschedule_result = Event.Reschedule_result

  type ('a, 'h) t = ('a, 'h) Event.t [@@deriving sexp_of]
  type t_unit = Event.t_unit [@@deriving sexp_of]

  let invariant = Event.invariant
  let abort = Event.abort
  let abort_exn = Event.abort_exn
  let abort_if_possible = Event.abort_if_possible
  let fired = Event.fired
  let scheduled_at t = Time_ns.to_time_float_round_nearest (Event.scheduled_at t)
  let at time = Event.at (Time_ns.of_time_float_round_nearest time)
  let after span = Event.after (Time_ns.Span.of_span_float_round_nearest span)

  let reschedule_at t time =
    Event.reschedule_at t (Time_ns.of_time_float_round_nearest time)
  ;;

  let reschedule_after t span =
    Event.reschedule_after t (Time_ns.Span.of_span_float_round_nearest span)
  ;;

  let run_at time f x = Event.run_at (Time_ns.of_time_float_round_nearest time) f x

  let run_after span f x =
    Event.run_after (Time_ns.Span.of_span_float_round_nearest span) f x
  ;;

  module Status = struct
    type ('a, 'h) t =
      | Aborted of 'a
      | Happened of 'h
      | Scheduled_at of Time.t
    [@@deriving sexp_of]
  end

  let status t : _ Status.t =
    match Event.status t with
    | Aborted a -> Aborted a
    | Happened h -> Happened h
    | Scheduled_at time -> Scheduled_at (Time_ns.to_time_float_round_nearest time)
  ;;
end

let at_varying_intervals ?stop f =
  at_varying_intervals ?stop (fun () -> Time_ns.Span.of_span_float_round_nearest (f ()))
;;

let at_intervals ?start ?stop span =
  let start = Option.map start ~f:Time_ns.of_time_float_round_nearest in
  at_intervals ?start ?stop (Time_ns.Span.of_span_float_round_nearest span)
;;

let every' ?start ?stop ?continue_on_error ?finished span f =
  every'
    ?start
    ?stop
    ?continue_on_error
    ?finished
    (Time_ns.Span.of_span_float_round_nearest span)
    f
;;

let every ?start ?stop ?continue_on_error span f =
  every ?start ?stop ?continue_on_error (Time_ns.Span.of_span_float_round_nearest span) f
;;

let run_at_intervals' ?start ?stop ?continue_on_error span f =
  let start = Option.map start ~f:Time_ns.of_time_float_round_nearest in
  run_at_intervals'
    ?start
    ?stop
    ?continue_on_error
    (Time_ns.Span.of_span_float_round_nearest span)
    f
;;

let run_at_intervals ?start ?stop ?continue_on_error span f =
  let start = Option.map start ~f:Time_ns.of_time_float_round_nearest in
  run_at_intervals
    ?start
    ?stop
    ?continue_on_error
    (Time_ns.Span.of_span_float_round_nearest span)
    f
;;
