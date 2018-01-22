(jbuild_version 1)

(alias ((name sexp) (deps (gdb_alternate2.rawsh.sexp))))

(rule (
  (targets (gdb_alternate2.rawsh.sexp))
  (deps    (gdb_alternate2.rawsh))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (pattern_polymorphic.ml.sexp))))

(rule (
  (targets (pattern_polymorphic.ml.sexp))
  (deps    (pattern_polymorphic.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (perf_report.rawsh.sexp))))

(rule (
  (targets (perf_report.rawsh.sexp))
  (deps    (perf_report.rawsh))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (gdb_alternate0.rawsh.sexp))))

(rule (
  (targets (gdb_alternate0.rawsh.sexp))
  (deps    (gdb_alternate0.rawsh))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (opam_switch.rawsh.sexp))))

(rule (
  (targets (opam_switch.rawsh.sexp))
  (deps    (opam_switch.rawsh))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (gdb_alternate1.rawsh.sexp))))

(rule (
  (targets (gdb_alternate1.rawsh.sexp))
  (deps    (gdb_alternate1.rawsh))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (jbuild.sexp))))

(rule (
  (targets (jbuild.sexp))
  (deps    (jbuild))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (gdb_alternate3.rawsh.sexp))))

(rule (
  (targets (gdb_alternate3.rawsh.sexp))
  (deps    (gdb_alternate3.rawsh))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (cmp.S.sexp))))

(rule (
  (targets (cmp.S.sexp))
  (deps    (cmp.S))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (instr_for_pattern_monomorphic_small.sh.sexp))))

(rule (
  (targets (instr_for_pattern_monomorphic_small.sh.sexp))
  (deps    (instr_for_pattern_monomorphic_small.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (compare_poly.ml.sexp))))

(rule (
  (targets (compare_poly.ml.sexp))
  (deps    (compare_poly.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (asm_from_compare_mono.sh.sexp))))

(rule (
  (targets (asm_from_compare_mono.sh.sexp))
  (deps    (asm_from_compare_mono.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (lambda_for_pattern_monomorphic_large.sh.sexp))))

(rule (
  (targets (lambda_for_pattern_monomorphic_large.sh.sexp))
  (deps (lambda_for_pattern_monomorphic_large.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (compare_poly_asm.S.sexp))))

(rule (
  (targets (compare_poly_asm.S.sexp))
  (deps    (compare_poly_asm.S))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (lambda_for_pattern_monomorphic_small.sh.sexp))))

(rule (
  (targets (lambda_for_pattern_monomorphic_small.sh.sexp))
  (deps (lambda_for_pattern_monomorphic_small.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (perf_record.rawsh.sexp))))

(rule (
  (targets (perf_record.rawsh.sexp))
  (deps    (perf_record.rawsh))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (lambda_for_pattern_polymorphic.sh.sexp))))

(rule (
  (targets (lambda_for_pattern_polymorphic.sh.sexp))
  (deps    (lambda_for_pattern_polymorphic.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (pattern_monomorphic_small.ml.sexp))))

(rule (
  (targets (pattern_monomorphic_small.ml.sexp))
  (deps    (pattern_monomorphic_small.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (pattern_monomorphic_large.ml.sexp))))

(rule (
  (targets (pattern_monomorphic_large.ml.sexp))
  (deps    (pattern_monomorphic_large.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

(alias ((name sexp) (deps (compare_mono.ml.sexp))))

(rule (
  (targets (compare_mono.ml.sexp))
  (deps    (compare_mono.ml))
  (fallback)
  (action (with-stdout-to ${@} (run rwo-build eval ${<})))))

