(jbuild_version 1)

(alias (
  (name cram)
  (deps (run_exn_cost.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name cram)
  (deps (run_exn_cost_notrace.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

