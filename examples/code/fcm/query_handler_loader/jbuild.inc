(jbuild_version 1)

(alias (
  (name cram)
  (deps (build_query_handler_loader.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

