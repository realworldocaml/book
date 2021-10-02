(executables
 (names
   bail
   check_basic
   check_located
   check_long
   compact
   duplicate_test_names
   empty_suite_name
   exception_in_test
   filter_all_tests
   invalid_arg_in_test
   long_test_case_name
   outside_runner
   tail_errors_limit
   tail_errors_unlimited
   unknown_option
 )
 (libraries alcotest alcotest.engine)
 (modules
   bail
   check_basic
   check_located
   check_long
   compact
   duplicate_test_names
   empty_suite_name
   exception_in_test
   filter_all_tests
   invalid_arg_in_test
   long_test_case_name
   outside_runner
   tail_errors_limit
   tail_errors_unlimited
   unknown_option
 )
)

(rule
 (target bail.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run %{dep:bail.exe})))))

(rule
 (target bail.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:bail.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff bail.expected bail.processed)))

(rule
 (target check_basic.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run %{dep:check_basic.exe})))))

(rule
 (target check_basic.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:check_basic.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff check_basic.expected check_basic.processed)))

(rule
 (target check_located.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run %{dep:check_located.exe})))))

(rule
 (target check_located.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:check_located.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff check_located.expected check_located.processed)))

(rule
 (target check_long.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run %{dep:check_long.exe})))))

(rule
 (target check_long.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:check_long.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff check_long.expected check_long.processed)))

(rule
 (target compact.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run %{dep:compact.exe})))))

(rule
 (target compact.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:compact.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff compact.expected compact.processed)))

(rule
 (target duplicate_test_names.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run %{dep:duplicate_test_names.exe})))))

(rule
 (target duplicate_test_names.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:duplicate_test_names.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff duplicate_test_names.expected duplicate_test_names.processed)))

(rule
 (target empty_suite_name.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run %{dep:empty_suite_name.exe})))))

(rule
 (target empty_suite_name.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:empty_suite_name.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff empty_suite_name.expected empty_suite_name.processed)))

(rule
 (target exception_in_test.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run %{dep:exception_in_test.exe})))))

(rule
 (target exception_in_test.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:exception_in_test.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff exception_in_test.expected exception_in_test.processed)))

(rule
 (target filter_all_tests.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run %{dep:filter_all_tests.exe} test bar 1)))))

(rule
 (target filter_all_tests.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:filter_all_tests.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff filter_all_tests.expected filter_all_tests.processed)))

(rule
 (target invalid_arg_in_test.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run %{dep:invalid_arg_in_test.exe})))))

(rule
 (target invalid_arg_in_test.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:invalid_arg_in_test.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff invalid_arg_in_test.expected invalid_arg_in_test.processed)))

(rule
 (target long_test_case_name.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run %{dep:long_test_case_name.exe})))))

(rule
 (target long_test_case_name.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:long_test_case_name.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff long_test_case_name.expected long_test_case_name.processed)))

(rule
 (target outside_runner.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run %{dep:outside_runner.exe})))))

(rule
 (target outside_runner.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:outside_runner.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff outside_runner.expected outside_runner.processed)))

(rule
 (target tail_errors_limit.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run %{dep:tail_errors_limit.exe})))))

(rule
 (target tail_errors_limit.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:tail_errors_limit.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff tail_errors_limit.expected tail_errors_limit.processed)))

(rule
 (target tail_errors_unlimited.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run %{dep:tail_errors_unlimited.exe})))))

(rule
 (target tail_errors_unlimited.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:tail_errors_unlimited.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff tail_errors_unlimited.expected tail_errors_unlimited.processed)))

(rule
 (target unknown_option.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run %{dep:unknown_option.exe} --dry-runn)))))

(rule
 (target unknown_option.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:unknown_option.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff unknown_option.expected unknown_option.processed)))
