open Core
open Poly
open Core_bench

module Entry = Ppx_bench_lib.Benchmark_accumulator.Entry

let x_library_inlining_warning ~run_without_inlining ~suppress_warnings =
  if not Version_util.x_library_inlining then begin
    if not suppress_warnings
    then
      Core.printf
        "Warning: X_LIBRARY_INLINING is not set to true, benchmarks may be inaccurate.\n%!";
    if not run_without_inlining then
      failwith "If you would like to run benchmarks, and are ok with getting inaccurate \
                results due to lack of cross library inlining, use the \
                -run-without-cross-library-inlining flag."
  end

(* The main function for the inline benchmarks *)
let run_benchmarks
      ~libname
      ~matching
      ~no_sexp:_
      ~run_config
      ~run_without_inlining
      ~suppress_warnings
      ~display_config
      ~analysis_configs
      ?save_to_file
      ()
  =
  x_library_inlining_warning ~run_without_inlining ~suppress_warnings;
  let _tbl, tests = Common.get_matching_tests ~libname matching in
  if List.is_empty tests
  then printf "No benchmarks to run!\n%!"
  else
    Bench.bench
      ~run_config
      ~analysis_configs
      ~display_config
      ?save_to_file
      ~libname:libname
      tests

let matching_param =
  let open Command.Param in
  flag "matching" (listed string)
    ~doc:"REGEX Include only benchmarks matching the REGEX."

let list_command ~libname =
  let open Command.Let_syntax in
  Command.basic ~summary:"list benchmark names"
    [%map_open
      let matching = matching_param
      in
      fun () ->
        let _, tests = Common.get_matching_tests ~libname matching in
        List.iter tests ~f:(fun test ->
          print_endline (Bench.Test.name test))
    ]

let command ~libname =
  let open Command.Let_syntax in
  Bench.make_command_ext
    ~summary:(sprintf "run inline benchmarks of %s now." libname)
    [%map_open
      let matching = matching_param
      and no_sexp =
        flag "no-sexp" no_arg
          ~doc:" Do not generate a benchmarks.sexp file (quicker)."
      and run_without_inlining =
        flag "run-without-cross-library-inlining" no_arg
          ~doc:" Run benchmarks even when compiled with X_LIBRARY_INLINING=false."
      and suppress_warnings =
        flag "suppress-warnings" no_arg
          ~doc:" Suppress warnings when clean output needed"
      in
      fun args ->
        let should_run =
          Option.value_map ~default:false ~f:((=) "TRUE") (Sys.getenv "BENCHMARKS_RUNNER")
        in
        if not should_run
        then failwith "Don't run directly, run using the benchmarks_runner script.";
        match args with
        | (analysis_configs, display_config, `Run (save_to_file, run_config)) ->
          run_benchmarks
            ~libname
            ~matching
            ~no_sexp
            ~run_config
            ~run_without_inlining
            ~suppress_warnings
            ~display_config
            ~analysis_configs
            ?save_to_file
            ()
        | (_analysis_configs, _display_config, `From_file _filenames) ->
          failwith "Loading saved files is not supported for inline executables."
    ]

let main ~libname = Command.run (command ~libname)
