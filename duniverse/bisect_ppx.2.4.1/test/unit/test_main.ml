(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



open OUnit2

let tests = "bisect_ppx" >::: [
  Test_report.tests;
  Test_send_to.tests;
  Test_instrument.tests;
  Test_attributes.tests;
  Test_warnings.tests;
  Test_line_number_directive.tests;
  Test_exclude_file.tests;
  Test_ppx_integration.tests;
  Test_thread_safety.tests;
  Test_ounit_integration.tests;
  Test_top_level.tests;
  Test_missing_files.tests;
  Test_open.tests;
  Test_include.tests;
  Test_mli.tests;
]

let () =
  run_test_tt_main tests
