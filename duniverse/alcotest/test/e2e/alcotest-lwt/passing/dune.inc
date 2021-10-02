(executables
 (names
   cli_options
 )
 (libraries alcotest alcotest-lwt lwt lwt.unix)
 (modules
   cli_options
 )
)

(rule
 (target cli_options.actual)
 (action
  (with-outputs-to %{target}
   (with-accepted-exit-codes (or 0 125)
    (run %{dep:cli_options.exe} --json)))))

(rule
 (target cli_options.processed)
 (action
  (with-outputs-to %{target}
   (run ../../strip_randomness.exe %{dep:cli_options.actual}))))

(rule
 (alias runtest)
 (package alcotest-lwt)
 (action
   (diff cli_options.expected cli_options.processed)))
