open Core

module Quota = Quota
module Test = Test
module Run_config = Run_config
module Display_column = Display_column
module Display_config = Display_config
module Variable = Variable
module Measurement = Measurement
module Analysis_config = Analysis_config
module Analysis_result = Analysis_result


let load_measurements ~filenames =
  List.map ~f:(fun filename -> Measurement.load ~filename) filenames

let save_measurements measurements ~to_filename =
  List.iter measurements ~f:(fun m ->
    Measurement.save m ~filename:(to_filename m))

let measure ?(run_config=Run_config.create ()) tests =
  let basic_tests = Test.expand tests in
  Benchmark.measure_all run_config basic_tests

let analyze ?(analysis_configs=Analysis_config.default) measurements =
  Analysis.analyze measurements analysis_configs

let display ?libname ?(display_config=Display_config.create ()) results =
  Display.display ?libname ~display_config results

let analyze_and_display ~measurements ?analysis_configs ?display_config ?libname () =
  let results = List.map ~f:(analyze ?analysis_configs) measurements in
  let results = List.filter_map results ~f:(function
    | Error err ->
      eprintf "Error %s\n%!" (Error.to_string_hum err);
      None
    | Ok r -> Some r)
  in
  display ?display_config ?libname results

let bench ?run_config ?analysis_configs ?display_config ?save_to_file ?libname tests =
  match Option.bind run_config ~f:Run_config.thin_overhead with
  | None ->
    let measurements = measure ?run_config tests in
    begin match save_to_file with
    | Some to_filename -> save_measurements measurements ~to_filename
    | None -> ()
    end;
    analyze_and_display ~measurements ?analysis_configs ?display_config ?libname ()
  | Some n ->
    let n = Int.max 0 n in
    (* Just run each test function n times. *)
    List.iter (Test.expand tests) ~f:(fun basic_test ->
      match Test.Basic_test.f basic_test with
      | Test.Basic_test.T f ->
        Verbosity.print_low "Running '%s' %i times\n" (Test.Basic_test.name basic_test) n;
        let f = f `init in
        for _ = 1 to n do
          ignore (f ())
        done
    )

let make_command tests =
  Bench_command.make
    ~bench
    ~analyze:(fun ~filenames ?analysis_configs ?display_config () ->
      let measurements = load_measurements ~filenames in
      analyze_and_display ~measurements ?analysis_configs ?display_config ())
    ~tests

let make_command_ext = Bench_command.make_ext


