If Bisect does not assign unique names to the internal instrumentation modules
it generates, including one module from another will trigger an error.

  $ echo "(lang dune 2.7)" > dune-project
  $ cat > dune <<'EOF'
  > (executable
  >  (name test)
  >  (modes byte)
  >  (instrumentation (backend bisect_ppx)))
  > EOF
  $ cat > test.ml <<'EOF'
  > include Helper
  > let () = ()
  > EOF
  $ cat > helper.ml <<'EOF'
  > let () = ()
  > EOF
  $ dune build --instrument-with bisect_ppx --display quiet
