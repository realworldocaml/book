open! Core
open! Timing_wheel

let start = Time_ns.epoch

(* In these benchmarks, the alarm times are chosen in such a way that computing
   the next alarm time is expensive because it involves traversing the entire level
   array. *)

(** This definition is copied from [lib/async_kernel/src/async_kernel_config.ml]. *)
module Alarm_precision = Timing_wheel.Alarm_precision

let alarm_precision, level_bits =
  match Word_size.word_size with
  | W32 -> Alarm_precision.about_one_millisecond, [ 10; 10; 9 ]
  | W64 -> Alarm_precision.(div about_one_millisecond ~pow2:3), [ 14; 15; 9; 6 ]
;;

let config =
  Timing_wheel.Config.create
    ~alarm_precision
    ~level_bits:(Timing_wheel.Level_bits.create_exn level_bits)
    ()
;;

let first_level_bits = List.hd_exn level_bits
let create () = Timing_wheel.create ~start ~config
let alarm_precision = Alarm_precision.to_span alarm_precision

let at_long =
  Time_ns.add
    start
    (Time_ns.Span.scale_int alarm_precision ((1 lsl first_level_bits) - 1))
;;

let at_short = Time_ns.add start (Time_ns.Span.scale_int alarm_precision 1)

let%bench_fun "[add] + [remove] next_alarm" =
  let t = create () in
  let (_alarm : _ Alarm.t) = add t ~at:at_long () in
  fun () -> remove t (add t ~at:at_short ())
;;

let%bench_fun "[add] + [remove] + [next_alarm_fires_at_exn]" =
  let t = create () in
  let (_alarm : _ Alarm.t) = add t ~at:at_long () in
  fun () ->
    remove t (add t ~at:at_short ());
    ignore (next_alarm_fires_at_exn t : Time_ns.t)
;;
