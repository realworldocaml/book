(jbuild_version 1)

(alias (
  (name runtest)
  (deps (run_basic_md5_as_filename.errsh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name runtest-all)
  (deps (run_basic_md5_as_filename.errsh (files_recursively_in .)))
  (action (
    progn (run cram --non-deterministic ${<}) (diff? ${<} ${<}.corrected)))))

