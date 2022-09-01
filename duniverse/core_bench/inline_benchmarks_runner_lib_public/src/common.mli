open! Core

val get_matching_tests
  :  libname:string
  -> string list
  -> Ppx_bench_lib.Benchmark_accumulator.Entry.t Int.Table.t
     * Core_bench.Bench.Test.t list

val get_matching_tests_no_dups
  :  libname:string
  -> string list
  -> Core_bench.Bench.Test.t list
