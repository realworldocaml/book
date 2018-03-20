(jbuild_version 1)

(alias (
  (name runtest)
  (deps (build_and_run_cal_add_interactive.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name runtest-all)
  (deps (build_and_run_cal_add_interactive.sh (files_recursively_in .)))
  (action (
    progn (run cram --non-deterministic ${<}) (diff? ${<} ${<}.corrected)))))

