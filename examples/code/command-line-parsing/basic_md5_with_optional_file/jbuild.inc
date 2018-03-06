(jbuild_version 1)

(alias (
  (name cram)
  (deps (basic_and_default_md5.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

