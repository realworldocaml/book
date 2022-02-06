Signal handlers are restored after dumping coverage, in the case that
bisect runs before an applications at_exit hook.

In this test, we instrument an application `at_exit_main.ml` that has
it's own at_exit hook. We ensure that bisect's hook is running before
the applications hook. To do so, we put the applications hook in a
separate file `at_exit_hook.ml` with coverage turned off: this means
that bisect's instrumentation won't kick in until after the
`at_exit.ml` has added the applications hook.

The applications forks. The child will immediately exit and then sleep
a few seconds in its at_exit hook. If allowed to sleep uninterrupted,
it outputs a message. However, the parent kills the child before it
has time to do so. If bisect has properly restored the default signal
handler, we should see no message and find two coverage traces:

  $ echo "(lang dune 2.7)" > dune-project
  $ cat > dune <<'EOF'
  > (executable
  >  (name at_exit_main)
  >  (preprocess (pps bisect_ppx --bisect-sigterm)))
  > EOF
  $ dune exec ./at_exit_main.exe
  $ ls bisect*.coverage | wc -l | sed 's/ *//'
  2
  $ bisect-ppx-report summary --verbose
  Info: found *.coverage files in './'
  Coverage: 6/6 (100.00%)
