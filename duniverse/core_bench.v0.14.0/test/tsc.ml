open Core
open Core_bench

let calibrator = force Time_stamp_counter.calibrator

let command = Bench.make_command [
  Bench.Test.create ~name:"Time.now" (fun () ->
    ignore (Time.now ()));

  Bench.Test.create ~name:"Calibrator.calibrate" (fun () ->
    ignore (Time_stamp_counter.Calibrator.calibrate calibrator));

  Bench.Test.create_group ~name:"Perf" [
    Bench.Test.create ~name:"TSC.now" (fun () ->
      ignore (Time_stamp_counter.now ()));

    (let c = Time_stamp_counter.now () in
     Bench.Test.create ~name:"TSC.to_time" (fun () ->
       ignore (Time_stamp_counter.to_time c ~calibrator)));

    Bench.Test.create ~name:"TSC.to_time (TSC.now ())" (fun () ->
      ignore (Time_stamp_counter.to_time (Time_stamp_counter.now ()) ~calibrator));

    (let c = Time_stamp_counter.now () in
     Bench.Test.create ~name:"TSC.to_nanos_since_epoch" (fun () ->
       ignore (Time_stamp_counter.to_time_ns c ~calibrator)));

    Bench.Test.create ~name:"TSC.to_nanos_since_epoch(TSC.now ())" (fun () ->
      ignore (Time_stamp_counter.to_time_ns (Time_stamp_counter.now ()) ~calibrator));
  ];
]
