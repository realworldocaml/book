(executables
 (names
   and_exit_false
   and_exit_true
   assert_and_verbose
   assert_not_printed
   basic
   check_basic
   cli_verbose
   compact
   empty_test_name
   filter_name
   filter_name_regex
   filter_range
   isatty
   json_output
   list_tests
   only_monadic_effects
   quick_only
   quick_only_regex
   separator_testname
   unicode_testname
   verbose_newlines
 )
 (libraries alcotest alcotest.stdlib_ext alcotest.engine)
 (modes native js)
 (modules
   and_exit_false
   and_exit_true
   assert_and_verbose
   assert_not_printed
   basic
   check_basic
   cli_verbose
   compact
   empty_test_name
   filter_name
   filter_name_regex
   filter_range
   isatty
   json_output
   list_tests
   only_monadic_effects
   quick_only
   quick_only_regex
   separator_testname
   unicode_testname
   verbose_newlines
 )
)

(rule
 (target and_exit_false.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run %{dep:and_exit_false.exe})))))

(rule
 (target and_exit_false.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:and_exit_false.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff and_exit_false.expected and_exit_false.processed)))

(rule
 (target and_exit_false-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run node %{dep:and_exit_false.bc.js})))))

(rule
 (target and_exit_false-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:and_exit_false-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff and_exit_false-js.expected and_exit_false-js.processed)))

(rule
 (target and_exit_true.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run %{dep:and_exit_true.exe})))))

(rule
 (target and_exit_true.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:and_exit_true.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff and_exit_true.expected and_exit_true.processed)))

(rule
 (target and_exit_true-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run node %{dep:and_exit_true.bc.js})))))

(rule
 (target and_exit_true-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:and_exit_true-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff and_exit_true-js.expected and_exit_true-js.processed)))

(rule
 (target assert_and_verbose.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run %{dep:assert_and_verbose.exe})))))

(rule
 (target assert_and_verbose.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:assert_and_verbose.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff assert_and_verbose.expected assert_and_verbose.processed)))

(rule
 (target assert_and_verbose-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run node %{dep:assert_and_verbose.bc.js})))))

(rule
 (target assert_and_verbose-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:assert_and_verbose-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff assert_and_verbose-js.expected assert_and_verbose-js.processed)))

(rule
 (target assert_not_printed.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run %{dep:assert_not_printed.exe})))))

(rule
 (target assert_not_printed.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:assert_not_printed.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff assert_not_printed.expected assert_not_printed.processed)))

(rule
 (target assert_not_printed-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run node %{dep:assert_not_printed.bc.js})))))

(rule
 (target assert_not_printed-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:assert_not_printed-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff assert_not_printed-js.expected assert_not_printed-js.processed)))

(rule
 (target basic.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run %{dep:basic.exe})))))

(rule
 (target basic.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:basic.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff basic.expected basic.processed)))

(rule
 (target basic-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run node %{dep:basic.bc.js})))))

(rule
 (target basic-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:basic-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff basic-js.expected basic-js.processed)))

(rule
 (target check_basic.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
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
   (with-accepted-exit-codes (or 0 125)
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
 (target cli_verbose.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run %{dep:cli_verbose.exe} --verbose)))))

(rule
 (target cli_verbose.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:cli_verbose.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff cli_verbose.expected cli_verbose.processed)))

(rule
 (target cli_verbose-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run node %{dep:cli_verbose.bc.js} --verbose)))))

(rule
 (target cli_verbose-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:cli_verbose-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff cli_verbose-js.expected cli_verbose-js.processed)))

(rule
 (target compact.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run %{dep:compact.exe} --compact)))))

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
   (with-accepted-exit-codes (or 0 125)
    (run node %{dep:compact.bc.js} --compact)))))

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
 (target empty_test_name.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run %{dep:empty_test_name.exe})))))

(rule
 (target empty_test_name.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:empty_test_name.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff empty_test_name.expected empty_test_name.processed)))

(rule
 (target empty_test_name-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run node %{dep:empty_test_name.bc.js})))))

(rule
 (target empty_test_name-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:empty_test_name-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff empty_test_name-js.expected empty_test_name-js.processed)))

(rule
 (target filter_name.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run %{dep:filter_name.exe})))))

(rule
 (target filter_name.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:filter_name.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff filter_name.expected filter_name.processed)))

(rule
 (target filter_name-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run node %{dep:filter_name.bc.js})))))

(rule
 (target filter_name-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:filter_name-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff filter_name-js.expected filter_name-js.processed)))

(rule
 (target filter_name_regex.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run %{dep:filter_name_regex.exe})))))

(rule
 (target filter_name_regex.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:filter_name_regex.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff filter_name_regex.expected filter_name_regex.processed)))

(rule
 (target filter_name_regex-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run node %{dep:filter_name_regex.bc.js})))))

(rule
 (target filter_name_regex-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:filter_name_regex-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff filter_name_regex-js.expected filter_name_regex-js.processed)))

(rule
 (target filter_range.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run %{dep:filter_range.exe} test main 0..1,1-3,5)))))

(rule
 (target filter_range.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:filter_range.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff filter_range.expected filter_range.processed)))

(rule
 (target filter_range-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run node %{dep:filter_range.bc.js} test main 0..1,1-3,5)))))

(rule
 (target filter_range-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:filter_range-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff filter_range-js.expected filter_range-js.processed)))

(rule
 (target isatty.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run %{dep:isatty.exe})))))

(rule
 (target isatty.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:isatty.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff isatty.expected isatty.processed)))

(rule
 (target isatty-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run node %{dep:isatty.bc.js})))))

(rule
 (target isatty-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:isatty-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff isatty-js.expected isatty-js.processed)))

(rule
 (target json_output.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run %{dep:json_output.exe})))))

(rule
 (target json_output.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:json_output.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff json_output.expected json_output.processed)))

(rule
 (target json_output-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run node %{dep:json_output.bc.js})))))

(rule
 (target json_output-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:json_output-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff json_output-js.expected json_output-js.processed)))

(rule
 (target list_tests.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run %{dep:list_tests.exe})))))

(rule
 (target list_tests.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:list_tests.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff list_tests.expected list_tests.processed)))

(rule
 (target list_tests-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run node %{dep:list_tests.bc.js})))))

(rule
 (target list_tests-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:list_tests-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff list_tests-js.expected list_tests-js.processed)))

(rule
 (target only_monadic_effects.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run %{dep:only_monadic_effects.exe})))))

(rule
 (target only_monadic_effects.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:only_monadic_effects.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff only_monadic_effects.expected only_monadic_effects.processed)))

(rule
 (target only_monadic_effects-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run node %{dep:only_monadic_effects.bc.js})))))

(rule
 (target only_monadic_effects-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:only_monadic_effects-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff only_monadic_effects-js.expected only_monadic_effects-js.processed)))

(rule
 (target quick_only.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run %{dep:quick_only.exe})))))

(rule
 (target quick_only.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:quick_only.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff quick_only.expected quick_only.processed)))

(rule
 (target quick_only-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run node %{dep:quick_only.bc.js})))))

(rule
 (target quick_only-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:quick_only-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff quick_only-js.expected quick_only-js.processed)))

(rule
 (target quick_only_regex.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run %{dep:quick_only_regex.exe})))))

(rule
 (target quick_only_regex.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:quick_only_regex.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff quick_only_regex.expected quick_only_regex.processed)))

(rule
 (target quick_only_regex-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run node %{dep:quick_only_regex.bc.js})))))

(rule
 (target quick_only_regex-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:quick_only_regex-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff quick_only_regex-js.expected quick_only_regex-js.processed)))

(rule
 (target separator_testname.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run %{dep:separator_testname.exe})))))

(rule
 (target separator_testname.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:separator_testname.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff separator_testname.expected separator_testname.processed)))

(rule
 (target separator_testname-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run node %{dep:separator_testname.bc.js})))))

(rule
 (target separator_testname-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:separator_testname-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff separator_testname-js.expected separator_testname-js.processed)))

(rule
 (target unicode_testname.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run %{dep:unicode_testname.exe})))))

(rule
 (target unicode_testname.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:unicode_testname.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff unicode_testname.expected unicode_testname.processed)))

(rule
 (target unicode_testname-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run node %{dep:unicode_testname.bc.js})))))

(rule
 (target unicode_testname-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:unicode_testname-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff unicode_testname-js.expected unicode_testname-js.processed)))

(rule
 (target verbose_newlines.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run %{dep:verbose_newlines.exe})))))

(rule
 (target verbose_newlines.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:verbose_newlines.actual}))))

(rule
 (alias runtest)
 (package alcotest)
 (action
   (diff verbose_newlines.expected verbose_newlines.processed)))

(rule
 (target verbose_newlines-js.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run node %{dep:verbose_newlines.bc.js})))))

(rule
 (target verbose_newlines-js.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:verbose_newlines-js.actual}))))

(rule
 (alias runtest-js)
 (package alcotest)
 (action
   (diff verbose_newlines-js.expected verbose_newlines-js.processed)))
