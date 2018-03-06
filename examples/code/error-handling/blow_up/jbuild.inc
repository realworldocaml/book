(jbuild_version 1)

(alias (
  (name cram)
  (deps (build_blow_up.errsh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name cram)
  (deps (build_blow_up_notrace.errsh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

