(jbuild_version 1)

(alias ((name sexp) (deps (run_map_vs_hash.sh.sexp))))

(rule (
  (targets (run_map_vs_hash.sh.sexp))
  (deps    (run_map_vs_hash.sh))
  (action (
    progn
    (bash "touch jbuild.inc")
    (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (map_vs_hash.ml.sexp))))

(rule (
  (targets (map_vs_hash.ml.sexp))
  (deps    (map_vs_hash.ml))
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

