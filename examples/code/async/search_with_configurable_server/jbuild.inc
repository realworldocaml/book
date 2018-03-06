(jbuild_version 1)

(alias (
  (name cram)
  (deps (run_search_with_configurable_server.errsh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

