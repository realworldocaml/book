open! Core_kernel
open! Import

module type Clock = Clock_intf.Clock
module type Clock_deprecated = Clock_intf.Clock_deprecated

module Scheduler = Scheduler1

let time_source () = (Scheduler.t ()).time_source |> Time_source.of_synchronous
let after span = Time_source.after (time_source ()) span
let at time = Time_source.at (time_source ()) time

let at_varying_intervals ?stop compute_span =
  Time_source.at_varying_intervals ?stop (time_source ()) compute_span
;;

let at_intervals ?start ?stop interval =
  Time_source.at_intervals ?start ?stop (time_source ()) interval
;;

let every' ?start ?stop ?continue_on_error ?finished span f =
  Time_source.every' ?start ?stop ?continue_on_error ?finished (time_source ()) span f
;;

let every ?start ?stop ?continue_on_error span f =
  Time_source.every ?start ?stop ?continue_on_error (time_source ()) span f
;;

let run_after span f a = Time_source.run_after (time_source ()) span f a
let run_at time f a = Time_source.run_at (time_source ()) time f a

let run_at_intervals ?start ?stop ?continue_on_error interval f =
  Time_source.run_at_intervals
    ?start
    ?stop
    ?continue_on_error
    (time_source ())
    interval
    f
;;

let run_at_intervals' ?start ?stop ?continue_on_error interval f =
  Time_source.run_at_intervals'
    ?start
    ?stop
    ?continue_on_error
    (time_source ())
    interval
    f
;;

let with_timeout span d = Time_source.with_timeout (time_source ()) span d

module Event = struct
  include Time_source.Event

  let after span = after (time_source ()) span
  let run_after span f a = run_after (time_source ()) span f a
  let at time = at (time_source ()) time
  let run_at time f z = run_at (time_source ()) time f z
end
