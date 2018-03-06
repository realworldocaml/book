(jbuild_version 1)

(alias (
  (name cram)
  (deps (run_basic_md5_with_custom_arg.errsh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

