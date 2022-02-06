Bisect's runtime can install a signal handler.

  $ echo "(lang dune 2.7)" > dune-project
  $ cat > dune <<'EOF'
  > (executables
  >  (names daemon)
  >  (preprocess (pps bisect_ppx --bisect-sigterm)))
  > EOF
  $ dune exec ./daemon.exe
  $ ls bisect*.coverage | wc -l | sed 's/ *//'
  2
  $ bisect-ppx-report summary --verbose
  Info: found *.coverage files in './'
  Coverage: 5/6 (83.33%)
  $ rm bisect*.coverage

An application instrumented with a signal handler will write coverage
data when terminating normally:

  $ echo "(lang dune 2.7)" > dune-project
  $ cat > dune <<'EOF'
  > (executables
  >  (names normal)
  >  (preprocess (pps bisect_ppx --bisect-sigterm)))
  > EOF
  $ cat > normal.ml <<'EOF'
  > let () = ()
  > EOF
  $ dune exec ./normal.exe
  $ bisect-ppx-report summary --verbose
  Info: found *.coverage files in './'
  Coverage: 0/0 (0.00%)
  $ rm bisect*.coverage
