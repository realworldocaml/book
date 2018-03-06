(jbuild_version 1)

(alias (
  (name cram)
  (deps (build_session_info.errsh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

