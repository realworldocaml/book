open Core
module Test = Test
module Run_config = Run_config
module Measurement = Measurement
module Analysis_config = Analysis_config

let save_measurements measurements ~to_filename =
  List.iter measurements ~f:(fun m -> Measurement.save m ~filename:(to_filename m))
;;

let measure ?(run_config = Run_config.create ()) tests ~measure_with =
  let basic_tests = Test.expand tests in
  measure_with run_config basic_tests
;;

let analyze ?(analysis_configs = Analysis_config.default) measurements =
  Analysis.analyze measurements analysis_configs
;;

let analyze_and_display
      ~measurements
      ?analysis_configs
      ?display_config
      ?libname
      ~display
      ()
  =
  let results = List.map ~f:(analyze ?analysis_configs) measurements in
  let results =
    List.filter_map results ~f:(function
      | Error err ->
        eprintf "Error %s\n%!" (Error.to_string_hum err);
        None
      | Ok r -> Some r)
  in
  display ?libname ?display_config results
;;

let bench
      ?run_config
      ?analysis_configs
      ?display_config
      ?save_to_file
      ?libname
      tests
      ~(measure_with : Run_config.t -> Test.Basic_test.t list -> Measurement.t list)
      ~display
  =
  match Option.bind run_config ~f:Run_config.thin_overhead with
  | None ->
    let measurements = measure ?run_config tests ~measure_with in
    (match save_to_file with
     | Some to_filename -> save_measurements measurements ~to_filename
     | None -> ());
    analyze_and_display
      ~measurements
      ?analysis_configs
      ?display_config
      ?libname
      ~display
      ()
  | Some n ->
    let n = Int.max 0 n in
    (* Just run each test function n times. *)
    List.iter (Test.expand tests) ~f:(fun basic_test ->
      match Test.Basic_test.f basic_test with
      | Test.Basic_test.T f ->
        Verbosity.print_low
          "Running '%s' %i times\n"
          (Test.Basic_test.name basic_test)
          n;
        let f = f `init in
        for _ = 1 to n do
          ignore (f () : (* existential type from GADT *) _)
        done)
;;


