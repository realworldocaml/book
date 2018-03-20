(jbuild_version 1)

(alias (
  (name runtest)
  (deps (run_search_with_timeout_no_leak_simple.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

(alias (
  (name runtest-all)
  (deps (run_search_with_timeout_no_leak_simple.sh (files_recursively_in .)))
  (action (
    progn (run cram --non-deterministic ${<}) (diff? ${<} ${<}.corrected)))))

