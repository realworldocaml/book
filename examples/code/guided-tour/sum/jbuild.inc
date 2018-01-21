(jbuild_version 1)

(alias ((name sexp) (deps (sum.rawsh.sexp))))

(rule (
  (targets (sum.rawsh.sexp))
  (deps    (sum.rawsh))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (sum.ml.sexp))))

(rule (
  (targets (sum.ml.sexp))
  (deps    (sum.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_sum.sh.sexp))))

(rule (
  (targets (build_sum.sh.sexp))
  (deps    (build_sum.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

