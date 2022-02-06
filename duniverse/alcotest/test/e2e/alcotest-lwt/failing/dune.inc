(executables
 (names
   async_failure
   fail_with
 )
 (libraries alcotest alcotest.stdlib_ext alcotest-lwt lwt lwt.unix)
 (modes native)
 (modules
   async_failure
   fail_with
 )
)

(rule
 (target async_failure.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run %{dep:async_failure.exe})))))

(rule
 (target async_failure.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:async_failure.actual}))))

(rule
 (alias runtest)
 (package alcotest-lwt)
 (action
   (diff async_failure.expected async_failure.processed)))

(rule
 (target fail_with.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 1 2 125)
    (run %{dep:fail_with.exe})))))

(rule
 (target fail_with.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:fail_with.actual}))))

(rule
 (alias runtest)
 (package alcotest-lwt)
 (action
   (diff fail_with.expected fail_with.processed)))
