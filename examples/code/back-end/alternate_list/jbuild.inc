(jbuild_version 1)

(alias (
  (name cram)
  (deps (run_alternate_list.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

