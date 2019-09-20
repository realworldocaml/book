  $ jbuilder runtest --dev
           run alias runtest (exit 2)
  (cd _build/default && ./.foo.inline-tests/run.exe inline-test-runner foo -source-tree-root . -diff-cmd -)
  File "test.ml", line 3, characters 0-71: rev threw
  (runtime-lib/runtime.ml.E "comparison failed"
    ((1 2 3) vs (3 2 1) (Loc test.ml:4:13))).
    Raised at file "src/import0.ml" (inlined), line 351, characters 22-32
    Called from file "runtime-lib/runtime.ml", line 28, characters 28-53
    Called from file "runtime-lib/runtime.ml", line 486, characters 15-19
    Called from file "runtime-lib/runtime.ml", line 327, characters 8-12
    Re-raised at file "runtime-lib/runtime.ml", line 330, characters 6-13
    Called from file "runtime-lib/runtime.ml", line 343, characters 15-52
    Called from file "runtime-lib/runtime.ml", line 430, characters 52-83
  
  FAILED 1 / 1 tests
@@ exit 1
