
(rule
 (alias bigger_quickcheck_test)
 (deps
  (source_tree ./correct/bigger_quickcheck_test)
  (package base)
  (package core_kernel)
  (package ppx_jane)
  (package stdio))
 (action
  (run dune build @all @runtest --root ./correct/bigger_quickcheck_test)))

(alias
 (name runtest)
 (deps (alias bigger_quickcheck_test)))

(rule
 (alias bigger_quickcheck_test_with_ppx)
 (deps
  (source_tree ./correct/bigger_quickcheck_test_with_ppx)
  (package base)
  (package core_kernel)
  (package ppx_jane)
  (package stdio))
 (action
  (run dune build @all @runtest --root ./correct/bigger_quickcheck_test_with_ppx)))

(alias
 (name runtest)
 (deps (alias bigger_quickcheck_test_with_ppx)))

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
  (run dune build @all @runtest --root ./correct/manual_property_test)))

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
  (run dune build @all @runtest --root ./correct/multi_block_expect_test)))

(alias
 (name runtest)
 (deps (alias multi_block_expect_test)))

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
  (run dune build @all @runtest --root ./correct/simple_expect_test)))

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
  (run dune build @all @runtest --root ./correct/simple_inline_test)))

(alias
 (name runtest)
 (deps (alias simple_inline_test)))

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
  (run dune build @all @runtest --root ./correct/soup_test_fixed)))

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
  (run dune build @all @runtest --root ./correct/trivial_expect_test_fixed)))

(alias
 (name runtest)
 (deps (alias trivial_expect_test_fixed)))
