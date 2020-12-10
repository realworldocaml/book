open Core
let () = Inline_benchmarks_public.Runner.main
           ~libname:(Option.value ~default:"<UNKNOWN_LIBRARY>" (Sys.getenv "BENCH_LIB"))
