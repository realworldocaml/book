(jbuild_version 1)

(alias (
  (name runtest)
  (deps (basic_md5_with_flags.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name runtest-all)
  (deps (basic_md5_with_flags.sh (files_recursively_in .)))
  (action (
    progn (run cram --non-deterministic ${<}) (diff? ${<} ${<}.corrected)))))

