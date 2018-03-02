(jbuild_version 1)

(alias ((name sexp) (deps (build_read_foo_better_errors.errsh.sexp))))

(rule (
  (targets (build_read_foo_better_errors.errsh.sexp))
  (deps (
    build_read_foo_better_errors.errsh
    jbuild
    jbuild.inc
    foo_broken_example.scm
    read_foo_better_errors.ml))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

