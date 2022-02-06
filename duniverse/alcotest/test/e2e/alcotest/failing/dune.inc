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
 (libraries alcotest alcotest.stdlib_ext alcotest.engine)
 (modes native js)
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
 (target bail-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run node %{dep:bail.bc.js})))))

(rule
 (target bail-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:bail-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff bail-js.expected bail-js.processed)))

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
 (target check_basic-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run node %{dep:check_basic.bc.js})))))

(rule
 (target check_basic-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:check_basic-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff check_basic-js.expected check_basic-js.processed)))

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
 (target check_located-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run node %{dep:check_located.bc.js})))))

(rule
 (target check_located-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:check_located-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff check_located-js.expected check_located-js.processed)))

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
 (target check_long-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run node %{dep:check_long.bc.js})))))

(rule
 (target check_long-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:check_long-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff check_long-js.expected check_long-js.processed)))

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
 (target compact-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run node %{dep:compact.bc.js})))))

(rule
 (target compact-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:compact-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff compact-js.expected compact-js.processed)))

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
 (target duplicate_test_names-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run node %{dep:duplicate_test_names.bc.js})))))

(rule
 (target duplicate_test_names-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:duplicate_test_names-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff duplicate_test_names-js.expected duplicate_test_names-js.processed)))

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
 (target empty_suite_name-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run node %{dep:empty_suite_name.bc.js})))))

(rule
 (target empty_suite_name-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:empty_suite_name-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff empty_suite_name-js.expected empty_suite_name-js.processed)))

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
 (target exception_in_test-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run node %{dep:exception_in_test.bc.js})))))

(rule
 (target exception_in_test-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:exception_in_test-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff exception_in_test-js.expected exception_in_test-js.processed)))

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
 (target filter_all_tests-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run node %{dep:filter_all_tests.bc.js} test bar 1)))))

(rule
 (target filter_all_tests-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:filter_all_tests-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff filter_all_tests-js.expected filter_all_tests-js.processed)))

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
 (target invalid_arg_in_test-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run node %{dep:invalid_arg_in_test.bc.js})))))

(rule
 (target invalid_arg_in_test-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:invalid_arg_in_test-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff invalid_arg_in_test-js.expected invalid_arg_in_test-js.processed)))

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
 (target long_test_case_name-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run node %{dep:long_test_case_name.bc.js})))))

(rule
 (target long_test_case_name-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:long_test_case_name-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff long_test_case_name-js.expected long_test_case_name-js.processed)))

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
 (target outside_runner-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run node %{dep:outside_runner.bc.js})))))

(rule
 (target outside_runner-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:outside_runner-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff outside_runner-js.expected outside_runner-js.processed)))

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
 (target tail_errors_limit-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run node %{dep:tail_errors_limit.bc.js})))))

(rule
 (target tail_errors_limit-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:tail_errors_limit-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff tail_errors_limit-js.expected tail_errors_limit-js.processed)))

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
 (target tail_errors_unlimited-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run node %{dep:tail_errors_unlimited.bc.js})))))

(rule
 (target tail_errors_unlimited-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:tail_errors_unlimited-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff tail_errors_unlimited-js.expected tail_errors_unlimited-js.processed)))

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

(rule
 (target unknown_option-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run node %{dep:unknown_option.bc.js} --dry-runn)))))

(rule
 (target unknown_option-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:unknown_option-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff unknown_option-js.expected unknown_option-js.processed)))
