(jbuild_version 1)

(alias (
  (name cram)
  (deps (build_test_interval.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

