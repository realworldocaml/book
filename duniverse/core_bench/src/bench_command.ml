(* This module makes the command line interface for bench. *)
open Core

type callback_bench
  = ?run_config:Run_config.t
  -> ?analysis_configs:Analysis_config.t list
  -> ?display_config:Display_config.t
  -> ?save_to_file:(Measurement.t -> string)
  -> ?libname:string
  -> Test.t list
  -> unit

type callback_load_analyze_and_display
  = filenames:string list
  -> ?analysis_configs:Analysis_config.t list
  -> ?display_config:Display_config.t
  -> unit
  -> unit

let wrapper_param =
  let open Command.Let_syntax in
  [%map_open
    let limit_width_to =
      flag "-width" (optional_with_default Defaults.limit_width_to int)
        ~doc:(sprintf "WIDTH width limit on column display (default %d)."
                Defaults.limit_width_to)
    and display_style =
      flag "-display" (optional_with_default Defaults.display_as_string string)
        ~doc:(sprintf
                "STYLE Table style (short, tall, line, blank or column). Default %s."
                Defaults.display_as_string)
    and verbosity =
      flag "-v" no_arg ~doc:" High verbosity level."
    and quota =
      flag "-quota" (optional_with_default Defaults.quota Quota.arg_type)
        ~doc:(sprintf "<INT>x|<SPAN> Quota allowed per test. May be a number of runs \
                       (e.g. 1000x or 1e6x) or a time span (e.g. 10s or 500ms). \
                       Default %s."
                (Quota.to_string Defaults.quota))
    and fork_each_benchmark =
      flag "-fork" no_arg ~doc:" Fork and run each benchmark in separate child-process"
    and show_all_values =
      flag "-all-values" no_arg ~doc:" Show all column values, including very small ones."
    and no_compactions =
      flag "-no-compactions" no_arg ~doc:" Disable GC compactions."
    and show_overheads =
      flag "-overheads" no_arg ~doc:" Show measurement overheads, when applicable."
    and sampling_type =
      choose_one ~if_nothing_chosen:(Default_to (`Geometric Defaults.geometric_scale))
        [ flag "-linear" (optional int)
            ~doc:"INCREMENT Use linear sampling to explore number of runs, example 1."
          |> map ~f:(Option.map ~f:(fun k -> `Linear k))
        ; flag "-geometric" (optional float)
            ~doc:(sprintf "SCALE Use geometric sampling. (default %.2f)"
                    Defaults.geometric_scale)
          |> map ~f:(Option.map ~f:(fun s -> `Geometric s))
        ]
    and save_sample_data =
      flag "-save" no_arg
        ~doc:" Save benchmark data to <test name>.txt files."
    and show_output_as_sexp =
      flag "-sexp" no_arg ~doc:" Output as sexp."
    and minimal_tables =
      flag "-ascii" no_arg ~doc:" Display data in simple ascii based tables."
    and reduced_bootstrap =
      flag "-reduced-bootstrap" no_arg
        ~doc:" Reduce the number of bootstrapping iterations"
    and show_absolute_ci =
      flag "-ci-absolute" no_arg
        ~doc:" Display 95% confidence interval in absolute numbers"
    and stabilize_gc_between_runs =
      flag "-stabilize-gc" no_arg
        ~doc:" Stabilize GC between each sample capture."
    and clear_columns =
      flag "-clear-columns" no_arg
        ~doc:" Don't display default columns. Only show \ user specified ones."
    and analyze_files =
      flag "-load" (listed Filename.arg_type)
        ~doc:"FILE Analyze previously saved data files and don't run tests. \
              [-load] can be specified multiple times."
    and regressions =
      flag "-regression" (listed string)
        ~doc:"REGR Specify additional regressions (See -? help). "
    and thin_overhead =
      flag "-thin-overhead" (optional float)
        ~doc:"INT If given, just run the test function(s) N times; skip \
              measurements and regressions. Float lexemes like \"1e6\" are allowed."
      |> map ~f:(Option.map ~f:Float.to_int)
    and anon_columns =
      anon (sequence ("COLUMN" %: Bench_command_column.arg))
    in
    fun ~main () ->
      let sanitize_name str =
        String.map str ~f:(fun c ->
          if Char.is_alphanum c || String.mem "-_." c
          then c
          else '_')
      in
      let display = Defaults.string_to_display display_style in
      let display, ascii_table =
        if minimal_tables
        then Ascii_table.Display.column_titles, true
        else display, false
      in
      let verbosity : Verbosity.t =
        if show_output_as_sexp
        then Quiet
        else
        if verbosity
        then High
        else Low
      in
      let columns =
        if clear_columns
        then []
        else Defaults.command_columns
      in
      let columns = columns @ anon_columns in
      let analysis_configs, columns =
        let f =
          let open Bench_command_column in
          function
          | Analysis analysis -> First analysis
          | Display_column col -> Second col
        in
        List.partition_map columns ~f
      in
      let analysis_configs = List.concat analysis_configs in
      let analysis_configs =
        let to_name i = sprintf " [%d]" (i+1) in
        analysis_configs @
        (List.mapi regressions
           ~f:(fun i reg ->
             let regression_name = to_name i in
             printf "Regression%s = %s\n%!" regression_name reg;
             Analysis_config.parse reg ~regression_name))
      in
      let analysis_configs =
        if reduced_bootstrap
        then List.map analysis_configs
               ~f:(Analysis_config.reduce_bootstrap
                     ~bootstrap_trials:Analysis_config.default_reduced_bootstrap_trials)
        else analysis_configs
      in
      let save =
        if save_sample_data then begin
          printf "Measurements will be saved.\n%!";
          let time_str = Time.format (Time.now ()) "%F-%R" ~zone:(force Time.Zone.local) in
          Some (fun meas ->
            let name = Measurement.name meas in
            let fn = sprintf "%s-%s-%s.txt"
                       (sanitize_name name)
                       time_str
                       (Quota.to_string quota)
            in
            printf "Saving to: %s.\n%!" fn;
            fn)
        end
        else None
      in
      let run_config =
        Run_config.create
          ~verbosity
          ~quota
          ~sampling_type
          ~stabilize_gc_between_runs
          ~no_compactions
          ~fork_each_benchmark
          ?thin_overhead
          ()
      in
      let display_config =
        Display_config.create
          ~limit_width_to
          ~show_samples:   (List.mem columns `Samples    ~equal:Display_column.equal)
          ~show_percentage:(List.mem columns `Percentage ~equal:Display_column.equal)
          ~show_speedup:   (List.mem columns `Speedup    ~equal:Display_column.equal)
          ~show_all_values
          ~show_absolute_ci
          ~show_overheads
          ~display
          ~ascii_table
          ~show_output_as_sexp
          ()
      in
      let configs =
        match analyze_files with
        | [] ->
          (analysis_configs, display_config, `Run (save, run_config))
        | filenames ->
          (analysis_configs, display_config, `From_file filenames)
      in
      main configs
  ]

let readme () = sprintf "\
Columns that can be specified are:
\t%s

Columns with no significant values will not be displayed. The
following columns will be displayed by default:
\t%s

Error Estimates
===============
To display error estimates, prefix the column name (or
regression) with a '+'. Example +time.

(1) R^2 is the fraction of the variance of the responder (such as
runtime) that is accounted for by the predictors (such as number of
runs).  More informally, it describes how good a fit we're getting,
with R^2 = 1 indicating a perfect fit and R^2 = 0 indicating a
horrible fit. Also see:
http://en.wikipedia.org/wiki/Coefficient_of_determination

(2) Bootstrapping is used to compute 95%% confidence intervals
for each estimate.

Because we expect runtime to be very highly correlated with number of
runs, values very close to 1 are typical; an R^2 value for 'time' that
is less than 0.99 should cause some suspicion, and a value less than
0.9 probably indicates either a shortage of data or that the data is
erroneous or peculiar in some way.

Specifying additional regressions
=================================
The builtin in columns encode common analysis that apply to most
functions. Bench allows the user to specify custom analysis to help
understand relationships specific to a particular function using the
flag \"-regression\" . It is worth noting that this feature requires
some understanding of both linear regression and how various quatities
relate to each other in the OCaml runtime.  To specify a regression
one must specify the responder variable and a command separated list
of predictor variables.

For example: +Time:Run,mjGC,Comp

which asks bench to estimate execution time using three predictors
namely the number of runs, major GCs and compaction stats and display
error estimates. Drop the prefix '+' to suppress error estimation. The
variables available for regression include:
\t%s
"
                  Bench_command_column.column_description_table
                  (String.concat ~sep:" " Defaults.columns_as_string)
                  (Variable.summarize ())

let make_ext ~summary main_param =
  let open Command.Let_syntax in
  Command.basic ~readme ~summary
    [%map_open
      let wrapper = wrapper_param
      and main    = main_param
      in
      wrapper ~main
    ]

let make
      ~(bench : callback_bench)
      ~(analyze : callback_load_analyze_and_display)
      ~(tests : Test.t list)
  =
  make_ext
    ~summary:(
      sprintf "Benchmark for %s"
        (String.concat ~sep:", "
           (List.map tests ~f:(fun test ->
              let len = List.length (Test.tests test) in
              if len = 1
              then Test.name test
              else sprintf "%s (%d tests)" (Test.name test) len))))
    (Command.Param.return (fun args ->
       match args with
       | (analysis_configs, display_config, `Run (save_to_file, run_config)) ->
         bench
           ~analysis_configs
           ~display_config
           ~run_config
           ?save_to_file
           tests
       | (analysis_configs, display_config, `From_file filenames) ->
         analyze
           ~analysis_configs
           ~display_config
           ~filenames
           ()))
