
(rule
 (alias bigger_quickcheck_test)
 (deps
  (source_tree ./correct/bigger_quickcheck_test)
  (package base)
  (package core)
  (package core_kernel)
  (package ppx_jane)
  (package stdio)
  (package timezone))
 (action
  (system "dune build @all @runtest --root ./correct/bigger_quickcheck_test")))

(alias
 (name runtest)
 (deps (alias bigger_quickcheck_test)))

(rule
 (alias bigger_quickcheck_test_with_ppx)
 (deps
  (source_tree ./correct/bigger_quickcheck_test_with_ppx)
  (package base)
  (package core)
  (package core_kernel)
  (package ppx_jane)
  (package stdio))
 (action
  (system "dune build @all @runtest --root ./correct/bigger_quickcheck_test_with_ppx")))

(alias
 (name runtest)
 (deps (alias bigger_quickcheck_test_with_ppx)))

(rule
 (alias echo_test_delay)
 (deps
  (source_tree ./correct/echo_test_delay)
  (package core)
  (package async)
  (package ppx_jane))
 (action
  (system "dune build @all @runtest --root ./correct/echo_test_delay")))

(alias
 (name runtest)
 (deps (alias echo_test_delay)))

(rule
 (alias echo_test_reconnect)
 (deps
  (source_tree ./correct/echo_test_reconnect)
  (package core)
  (package async)
  (package ppx_jane))
 (action
  (system "dune build @all @runtest --root ./correct/echo_test_reconnect")))

(alias
 (name runtest)
 (deps (alias echo_test_reconnect)))

(rule
 (alias manual_property_test)
 (deps
  (source_tree ./correct/manual_property_test)
  (package base)
  (package bin_prot)
  (package ppx_jane)
  (package stdio)
  (package typerep))
 (action
  (system "dune build @all @runtest --root ./correct/manual_property_test")))

(alias
 (name runtest)
 (deps (alias manual_property_test)))

(rule
 (alias multi_block_expect_test)
 (deps
  (source_tree ./correct/multi_block_expect_test)
  (package base)
  (package ppx_jane)
  (package stdio))
 (action
  (system "dune build @all @runtest --root ./correct/multi_block_expect_test")))

(alias
 (name runtest)
 (deps (alias multi_block_expect_test)))

(rule
 (alias rate_limiter_fixed)
 (deps
  (source_tree ./correct/rate_limiter_fixed)
  (package core)
  (package base)
  (package bin_prot)
  (package ppx_jane)
  (package stdio)
  (package core))
 (action
  (system "dune build @all @runtest --root ./correct/rate_limiter_fixed")))

(alias
 (name runtest)
 (deps (alias rate_limiter_fixed)))

(rule
 (alias rate_limiter_show_bug)
 (deps
  (source_tree ./correct/rate_limiter_show_bug)
  (package core)
  (package base)
  (package bin_prot)
  (package ppx_jane)
  (package stdio)
  (package core))
 (action
  (system "dune build @all @runtest --root ./correct/rate_limiter_show_bug")))

(alias
 (name runtest)
 (deps (alias rate_limiter_show_bug)))

(rule
 (alias simple_expect_test)
 (deps
  (source_tree ./correct/simple_expect_test)
  (package base)
  (package bin_prot)
  (package ppx_jane)
  (package stdio)
  (package typerep))
 (action
  (system "dune build @all @runtest --root ./correct/simple_expect_test")))

(alias
 (name runtest)
 (deps (alias simple_expect_test)))

(rule
 (alias simple_inline_test)
 (deps
  (source_tree ./correct/simple_inline_test)
  (package base)
  (package bin_prot)
  (package ppx_jane)
  (package stdio)
  (package typerep))
 (action
  (system "dune build @all @runtest --root ./correct/simple_inline_test")))

(alias
 (name runtest)
 (deps (alias simple_inline_test)))

(rule
 (alias soup_test_corrected)
 (deps
  (source_tree ./correct/soup_test_corrected)
  (package base)
  (package bin_prot)
  (package lambdasoup)
  (package ppx_jane)
  (package stdio)
  (package typerep)
  (package uri))
 (action
  (system "dune build @all @runtest --root ./correct/soup_test_corrected")))

(alias
 (name runtest)
 (deps (alias soup_test_corrected)))

(rule
 (alias soup_test_fixed)
 (deps
  (source_tree ./correct/soup_test_fixed)
  (package base)
  (package bin_prot)
  (package lambdasoup)
  (package ppx_jane)
  (package stdio)
  (package typerep)
  (package uri))
 (action
  (system "dune build @all @runtest --root ./correct/soup_test_fixed")))

(alias
 (name runtest)
 (deps (alias soup_test_fixed)))

(rule
 (alias trivial_expect_test_fixed)
 (deps
  (source_tree ./correct/trivial_expect_test_fixed)
  (package base)
  (package bin_prot)
  (package ppx_jane)
  (package stdio)
  (package typerep))
 (action
  (system "dune build @all @runtest --root ./correct/trivial_expect_test_fixed")))

(alias
 (name runtest)
 (deps (alias trivial_expect_test_fixed)))
