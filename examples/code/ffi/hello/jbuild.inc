(jbuild_version 1)

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_hello.sh.sexp))))

(rule (
  (targets (build_hello.sh.sexp))
  (deps    (build_hello.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (hello.ml.sexp))))

(rule (
  (targets (hello.ml.sexp))
  (deps    (hello.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

