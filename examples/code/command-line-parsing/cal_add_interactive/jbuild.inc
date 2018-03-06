(jbuild_version 1)

(alias (
  (name cram)
  (deps (build_and_run_cal_add_interactive.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

