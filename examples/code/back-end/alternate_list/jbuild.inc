(jbuild_version 1)

(alias ((name sexp) (deps (alternate_list.ml.sexp))))

(rule (
  (targets (alternate_list.ml.sexp))
  (deps    (alternate_list.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (run_alternate_list.sh.sexp))))

(rule (
  (targets (run_alternate_list.sh.sexp))
  (deps    (run_alternate_list.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

