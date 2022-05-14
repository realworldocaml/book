open Core
module My_display = Display
open Core_bench_internals
module Quota = Quota
module Test = Test
module Run_config = Run_config
module Display_config = Display_config
module Variable = Variable
module Measurement = Measurement
module Analysis_config = Analysis_config
module Analysis_result = Analysis_result
include Core_bench_internals.Bench

let load_measurements ~filenames =
  List.map ~f:(fun filename -> Measurement.load ~filename) filenames
;;

let measure = Bench.measure ~measure_with:Benchmark.measure_all

let display ?libname ?(display_config = Display_config.create ()) =
  My_display.display ?libname ~display_config
;;

let analyze_and_display = Bench.analyze_and_display ~display
let bench = Bench.bench ~measure_with:Benchmark.measure_all ~display

let make_command tests =
  Bench_command.make
    ~bench
    ~analyze:(fun ~filenames ?analysis_configs ?display_config () ->
      let measurements = load_measurements ~filenames in
      analyze_and_display ~measurements ?analysis_configs ?display_config ())
    ~tests
;;

let make_command_ext = Bench_command.make_ext
