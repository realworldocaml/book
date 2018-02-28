(jbuild_version 1)

(alias ((name sexp) (deps (instr_for_pattern_monomorphic_small.sh.sexp))))

(rule (
  (targets (instr_for_pattern_monomorphic_small.sh.sexp))
  (deps    (instr_for_pattern_monomorphic_small.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

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

(alias ((name sexp) (deps (lambda_for_pattern_monomorphic_small.sh.sexp))))

(rule (
  (targets (lambda_for_pattern_monomorphic_small.sh.sexp))
  (deps (lambda_for_pattern_monomorphic_small.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

(alias ((name sexp) (deps (lambda_for_pattern_polymorphic.sh.sexp))))

(rule (
  (targets (lambda_for_pattern_polymorphic.sh.sexp))
  (deps    (lambda_for_pattern_polymorphic.sh))
  (fallback)
  (action (setenv TERM dumb (with-stdout-to ${@} (run rwo-build eval ${<}))))))

