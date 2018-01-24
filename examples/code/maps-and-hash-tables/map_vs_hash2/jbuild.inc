(jbuild_version 1)

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (run_map_vs_hash2.sh.sexp))))

(rule (
  (targets (run_map_vs_hash2.sh.sexp))
  (deps (run_map_vs_hash2.sh map_vs_hash2.ml jbuild jbuild.inc))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (map_vs_hash2.ml.sexp))))

(rule (
  (targets (map_vs_hash2.ml.sexp))
  (deps    (map_vs_hash2.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

