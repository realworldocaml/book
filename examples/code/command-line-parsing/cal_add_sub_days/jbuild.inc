(jbuild_version 1)

(alias (
  (name cram)
  (deps (cal_add_sub_days.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

