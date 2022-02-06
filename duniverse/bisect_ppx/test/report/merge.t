Merge two files

  $ echo "(lang dune 2.7)" > dune-project
  $ cat > dune <<'EOF'
  > (executables
  >  (names test_merge1 test_merge2)
  >  (instrumentation (backend bisect_ppx)))
  > EOF
  $ dune exec ./test_merge1.exe --instrument-with bisect_ppx
  $ bisect-ppx-report summary --per-file
   16.67 %   1/6   merge.ml
  100.00 %   2/2   test_merge1.ml
   37.50 %   3/8   Project coverage
  $ dune exec ./test_merge2.exe --instrument-with bisect_ppx
  $ bisect-ppx-report summary --per-file
   33.33 %   2/6    merge.ml
  100.00 %   2/2    test_merge1.ml
  100.00 %   2/2    test_merge2.ml
   60.00 %   6/10   Project coverage
  $ bisect-ppx-report merge merged.temp
  $ rm -rf _build; rm *.coverage; mv merged.temp merged.coverage
  $ bisect-ppx-report summary --per-file
   33.33 %   2/6    merge.ml
  100.00 %   2/2    test_merge1.ml
  100.00 %   2/2    test_merge2.ml
   60.00 %   6/10   Project coverage
