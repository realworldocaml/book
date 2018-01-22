(jbuild_version 1)

(alias ((name sexp) (deps (simple_build.sh.sexp))))

(rule (
  (targets (simple_build.sh.sexp))
  (deps    (simple_build.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (simple_build_fail.errsh.sexp))))

(rule (
  (targets (simple_build_fail.errsh.sexp))
  (deps    (simple_build_fail.errsh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (freq.ml.sexp))))

(rule (
  (targets (freq.ml.sexp))
  (deps    (freq.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

