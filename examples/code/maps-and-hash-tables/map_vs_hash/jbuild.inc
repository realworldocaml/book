(jbuild_version 1)

(alias (
  (name cram)
  (deps (run_map_vs_hash.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

