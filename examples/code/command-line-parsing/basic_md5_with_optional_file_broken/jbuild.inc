(jbuild_version 1)

(alias (
  (name runtest)
  (deps (
    build_basic_md5_with_optional_file_broken.errsh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name runtest-all)
  (deps (
    build_basic_md5_with_optional_file_broken.errsh (files_recursively_in .)))
  (action (
    progn (run cram --non-deterministic ${<}) (diff? ${<} ${<}.corrected)))))

