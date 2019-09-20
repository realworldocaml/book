  $ jbuilder runtest --dev
           run alias runtest (exit 2)
  (cd _build/default && ./.foo.inline-tests/run.exe inline-test-runner foo -source-tree-root . -diff-cmd -)
  File "test.ml", line 3, characters 0-185: negation flips the sign threw
  ("random input" (value -4611686018427387904)
    (error
      ((runtime-lib/runtime.ml.E "comparison failed"
         (Neg vs Pos (Loc test.ml:6:19)))
         "Raised at file \"src/import0.ml\" (inlined), line 351, characters 22-32\
        \nCalled from file \"runtime-lib/runtime.ml\", line 28, characters 28-53\
        \nCalled from file \"src/or_error.ml\", line 66, characters 9-15\
        \n"))).
    Raised at file "src/import0.ml" (inlined), line 351, characters 22-32
    Called from file "src/error.ml" (inlined), line 9, characters 14-30
    Called from file "src/or_error.ml", line 74, characters 17-32
    Called from file "runtime-lib/runtime.ml", line 486, characters 15-19
    Called from file "runtime-lib/runtime.ml", line 327, characters 8-12
    Re-raised at file "runtime-lib/runtime.ml", line 330, characters 6-13
    Called from file "runtime-lib/runtime.ml", line 343, characters 15-52
    Called from file "runtime-lib/runtime.ml", line 430, characters 52-83
  
  FAILED 1 / 1 tests
@@ exit 1
