(jbuild_version 1)

(alias (
  (name cram)
  (deps (basic_md5_with_flags.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

