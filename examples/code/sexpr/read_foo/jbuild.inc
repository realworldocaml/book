(jbuild_version 1)

(alias ((name sexp) (deps (build_read_foo.errsh.sexp))))

(rule (
  (targets (build_read_foo.errsh.sexp))
  (deps (
    build_read_foo.errsh foo_broken_example.scm jbuild jbuild.inc read_foo.ml))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

