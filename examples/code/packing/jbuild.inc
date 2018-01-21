(jbuild_version 1)

(alias ((name sexp) (deps (B.ml.sexp))))

(rule (
  (targets (B.ml.sexp))
  (deps    (B.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (test.ml.sexp))))

(rule (
  (targets (test.ml.sexp))
  (deps    (test.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (build_test.sh.sexp))))

(rule (
  (targets (build_test.sh.sexp))
  (deps    (build_test.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (show_files.sh.sexp))))

(rule (
  (targets (show_files.sh.sexp))
  (deps    (show_files.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (A.ml.sexp))))

(rule (
  (targets (A.ml.sexp))
  (deps    (A.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

