
(rule
 (alias better_echo)
 (deps
  (source_tree ./correct/better_echo)
  (package core)
  (package async))
 (action
  (run dune build @all @runtest --root ./correct/better_echo)))

(alias
 (name runtest)
 (deps (alias better_echo)))

(rule
 (alias echo)
 (deps
  (source_tree ./correct/echo)
  (package core)
  (package async))
 (action
  (run dune build @all @runtest --root ./correct/echo)))

(alias
 (name runtest)
 (deps (alias echo)))

(rule
 (alias native_code_log_delays)
 (deps
  (source_tree ./correct/native_code_log_delays)
  (package core)
  (package async))
 (action
  (run dune build @all @runtest --root ./correct/native_code_log_delays)))

(alias
 (name runtest)
 (deps (alias native_code_log_delays)))

(rule
 (alias search)
 (deps
  (source_tree ./correct/search)
  (package core)
  (package async)
  (package cohttp-async)
  (package yojson)
  (package textwrap))
 (action
  (run dune build @all @runtest --root ./correct/search)))

(alias
 (name runtest)
 (deps (alias search)))

(rule
 (alias search_out_of_order)
 (deps
  (source_tree ./correct/search_out_of_order)
  (package core)
  (package async)
  (package cohttp-async)
  (package yojson)
  (package textwrap))
 (action
  (run dune build @all @runtest --root ./correct/search_out_of_order)))

(alias
 (name runtest)
 (deps (alias search_out_of_order)))

(rule
 (alias search_with_configurable_server)
 (deps
  (source_tree ./correct/search_with_configurable_server)
  (package cohttp-async)
  (package yojson)
  (package textwrap))
 (action
  (run dune build @all @runtest --root ./correct/search_with_configurable_server)))

(alias
 (name runtest)
 (deps (alias search_with_configurable_server)))

(rule
 (alias search_with_error_handling)
 (deps
  (source_tree ./correct/search_with_error_handling)
  (package cohttp-async)
  (package yojson)
  (package textwrap))
 (action
  (run dune build @all @runtest --root ./correct/search_with_error_handling)))

(alias
 (name runtest)
 (deps (alias search_with_error_handling)))

(rule
 (alias search_with_timeout)
 (deps
  (source_tree ./correct/search_with_timeout)
  (package core)
  (package async)
  (package cohttp-async)
  (package yojson)
  (package textwrap))
 (action
  (run dune build @all @runtest --root ./correct/search_with_timeout)))

(alias
 (name runtest)
 (deps (alias search_with_timeout)))

(rule
 (alias search_with_timeout_no_leak)
 (deps
  (source_tree ./correct/search_with_timeout_no_leak)
  (package cohttp-async)
  (package yojson)
  (package textwrap))
 (action
  (run dune build @all @runtest --root ./correct/search_with_timeout_no_leak)))

(alias
 (name runtest)
 (deps (alias search_with_timeout_no_leak)))

(rule
 (alias search_with_timeout_no_leak_simple)
 (deps
  (source_tree ./correct/search_with_timeout_no_leak_simple)
  (package cohttp-async)
  (package yojson)
  (package textwrap))
 (action
  (run dune build @all @runtest --root ./correct/search_with_timeout_no_leak_simple)))

(alias
 (name runtest)
 (deps (alias search_with_timeout_no_leak_simple)))
