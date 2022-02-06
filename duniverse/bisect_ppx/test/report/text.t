  $ echo "(lang dune 2.7)" > dune-project
  $ cat > dune <<'EOF'
  > (executable
  >  (name test)
  >  (instrumentation (backend bisect_ppx)))
  > EOF
  $ dune exec ./test.exe --instrument-with bisect_ppx
  $ bisect-ppx-report summary --verbose
  Info: found *.coverage files in './'
  Coverage: 2/6 (33.33%)
