(jbuild_version 1)

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (foo_broken_example.scm.sexp))))

(rule (
  (targets (foo_broken_example.scm.sexp))
  (deps    (foo_broken_example.scm))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (read_foo.ml.sexp))))

(rule (
  (targets (read_foo.ml.sexp))
  (deps    (read_foo.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_read_foo.errsh.sexp))))

(rule (
  (targets (build_read_foo.errsh.sexp))
  (deps (
    build_read_foo.errsh foo_broken_example.scm jbuild jbuild.inc read_foo.ml))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

