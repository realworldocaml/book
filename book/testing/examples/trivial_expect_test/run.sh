  $ jbuilder runtest --dev
       patdiff (internal) (exit 1)
  (cd _build/default && /home/yminsky/.opam/default/bin/patdiff -keep-whitespace -location-style omake -ascii test.ml test.ml.corrected)
  ------ test.ml
  ++++++ test.ml.corrected
  File "test.ml", line 5, characters 0-1:
   |open! Base
   |open! Stdio
   |
   |let%expect_test "trivial" =
  -|  print_endline "Hello World!"
  +|  print_endline "Hello World!";
  +|  [%expect {| Hello World! |}]
@@ exit 1
