Missing files trigger a neat error.

  $ echo "(lang dune 2.7)" > dune-project
  $ cat > dune <<'EOF'
  > (executable
  >  (name test)
  >  (instrumentation (backend bisect_ppx)))
  > EOF
  $ dune exec ./test.exe --instrument-with bisect_ppx
  $ rm -rf _build
  $ mv test.ml test2.ml
  $ bisect-ppx-report html
  Error: cannot find source file 'test.ml' in:
    - .
    - ./_build/default
  Hint: consider passing --ignore-missing-files.
  [1]


--ignore-missing-files turns this error into a warning.

  $ echo "(lang dune 2.7)" > dune-project
  $ cat > dune <<'EOF'
  > (executable
  >  (name test)
  >  (instrumentation (backend bisect_ppx)))
  > EOF
  $ mv test2.ml test.ml
  $ dune exec ./test.exe --instrument-with bisect_ppx
  $ rm -rf _build
  $ mv test.ml test2.ml
  $ bisect-ppx-report html --ignore-missing-files --verbose
  Info: found *.coverage files in './'
  Info: cannot find source file 'test.ml' in:
    - .
    - ./_build/default
  Info: Writing index file...


The warning is visible only when --verbose is provided.

  $ echo "(lang dune 2.7)" > dune-project
  $ cat > dune <<'EOF'
  > (executable
  >  (name test)
  >  (instrumentation (backend bisect_ppx)))
  > EOF
  $ mv test2.ml test.ml
  $ dune exec ./test.exe --instrument-with bisect_ppx
  $ rm -rf _build
  $ mv test.ml test2.ml
  $ bisect-ppx-report html --ignore-missing-files
