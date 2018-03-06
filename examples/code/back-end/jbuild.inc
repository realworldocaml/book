(jbuild_version 1)

(alias (
  (name cram)
  (deps (instr_for_pattern_monomorphic_small.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name cram)
  (deps (asm_from_compare_mono.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name cram)
  (deps (lambda_for_pattern_monomorphic_large.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name cram)
  (deps (lambda_for_pattern_monomorphic_small.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name cram)
  (deps (lambda_for_pattern_polymorphic.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

