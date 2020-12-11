If Bisect's instrumentation helpers aren't properly named/structured in modules,
opening a user's module can shadow one module's instrumentation helpers by
another's. This test relies on OCaml's shadowing warnings to make sure that
shadowing is not occurring.

  $ echo "(lang dune 2.7)" > dune-project
  $ cat > dune <<'EOF'
  > (executable
  >  (name test)
  >  (modes byte)
  >  (instrumentation (backend bisect_ppx)))
  > EOF
  $ cat > test.ml <<'EOF'
  > [@@@ocaml.warning "+44"]
  > open Helper
  > let f () = g ()
  > EOF
  $ cat > helper.ml <<'EOF'
  > let g () = ()
  > EOF
  $ dune build --instrument-with bisect_ppx --display quiet
