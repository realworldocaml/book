Reporter fails to create the output directory.

  $ echo "(lang dune 2.7)" > dune-project
  $ cat > dune <<'EOF'
  > (executable
  >  (name test)
  >  (instrumentation (backend bisect_ppx)))
  > EOF
  $ dune exec ./test.exe --instrument-with bisect_ppx
  $ touch foo
  $ bisect-ppx-report html -o foo/bar/
  Error: cannot create directory 'foo/bar/': Not a directory
  [1]


Reporter fails to create intermediate directory.

  $ echo "(lang dune 2.7)" > dune-project
  $ cat > dune <<'EOF'
  > (executable
  >  (name test)
  >  (instrumentation (backend bisect_ppx)))
  > EOF
  $ dune exec ./test.exe --instrument-with bisect_ppx
  $ touch foo
  $ bisect-ppx-report html -o foo/bar/baz/
  Error: cannot create directory 'foo/bar': Not a directory
  [1]
