open! Core
open! Timing_wheel

let start = Time_ns.of_string_with_utc_offset "2020-12-25 12:10Z"
let alarm_precision = Alarm_precision.about_one_millisecond
let create () = create ~config:(Config.create () ~alarm_precision) ~start

let%bench_fun "[interval_num]" =
  let t = create () in
  let time = Time_ns.add start Time_ns.Span.second in
  fun () -> interval_num t time
;;

let%bench_fun "[advance_clock]" =
  let t = create () in
  fun () ->
    advance_clock
      t
      ~to_:(Time_ns.add (now t) Time_ns.Span.nanosecond)
      ~handle_fired:ignore
;;

let%bench_fun "[add] + [remove]" =
  let t = create () in
  let at = Time_ns.add start Time_ns.Span.second in
  fun () -> remove t (add t ~at ())
;;
