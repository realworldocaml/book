(jbuild_version 1)

(alias (
  (name cram)
  (deps (build.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

