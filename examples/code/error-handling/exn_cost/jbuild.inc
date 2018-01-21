(jbuild_version 1)

(alias ((name sexp) (deps (exn_cost.ml.sexp))))

(rule (
  (targets (exn_cost.ml.sexp))
  (deps    (exn_cost.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (run_exn_cost.sh.sexp))))

(rule (
  (targets (run_exn_cost.sh.sexp))
  (deps    (run_exn_cost.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (run_exn_cost_notrace.sh.sexp))))

(rule (
  (targets (run_exn_cost_notrace.sh.sexp))
  (deps    (run_exn_cost_notrace.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

