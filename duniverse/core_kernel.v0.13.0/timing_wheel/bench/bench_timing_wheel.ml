open! Core_kernel
open! Timing_wheel

let create () =
  create
    ~config:(Config.create () ~alarm_precision:Alarm_precision.about_one_millisecond)
    ~start:(Time_ns.now ())
;;

let%bench_fun "[interval_num]" =
  let t = create () in
  let time = Time_ns.now () in
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
  let at = Time_ns.now () in
  fun () -> remove t (add t ~at ())
;;
