(jbuild_version 1)

(alias (
  (name cram)
  (deps (build_short_parser.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

