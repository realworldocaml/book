Bisect's runtime does not clobber the global random number generator state.

  $ echo "(lang dune 2.7)" > dune-project
  $ cat > dune <<'EOF'
  > (executable
  >  (name random_test)
  >  (instrumentation (backend bisect_ppx)))
  > EOF
  $ cat > random_test.ml <<'EOF'
  > let () =
  >   Random.int 1000 |> string_of_int |> print_endline
  > EOF
  $ dune exec ./random_test.exe > pristine
  $ dune exec ./random_test.exe --instrument-with bisect_ppx > 1
  $ dune exec ./random_test.exe --instrument-with bisect_ppx > 2
  $ diff pristine 1
  $ diff pristine 2
