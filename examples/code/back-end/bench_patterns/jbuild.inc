(jbuild_version 1)

(alias (
  (name cram)
  (deps (run_bench_patterns.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

