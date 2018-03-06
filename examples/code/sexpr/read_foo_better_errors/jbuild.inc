(jbuild_version 1)

(alias (
  (name cram)
  (deps (build_read_foo_better_errors.errsh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

