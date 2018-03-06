(jbuild_version 1)

(alias (
  (name cram)
  (deps (build_cal_append_broken.errsh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

