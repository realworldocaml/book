(jbuild_version 1)

(alias (
  (name runtest)
  (deps (build_read_foo_better_errors.errsh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name runtest-all)
  (deps (build_read_foo_better_errors.errsh (files_recursively_in .)))
  (action (
    progn (run cram --non-deterministic ${<}) (diff? ${<} ${<}.corrected)))))

