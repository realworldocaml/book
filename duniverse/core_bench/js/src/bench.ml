open! Core
module My_display = Display
open Core_bench_internals
module Quota = Quota
module Test = Test
module Run_config = Run_config
module Display_column = Display_column
module Display_config = Display_config
module Variable = Variable
module Measurement = Measurement
module Analysis_config = Analysis_config
module Analysis_result = Analysis_result

include Core_bench_internals.Bench

let measure = Bench.measure ~measure_with:Benchmark.measure_all

let display ?libname ?(display_config = Display_config.create ()) =
  My_display.display ?libname ~display_config
;;

let bench = Bench.bench ~measure_with:Benchmark.measure_all ~display
