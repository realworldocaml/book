(jbuild_version 1)

(alias (
  (name cram)
  (deps (run_parse_book.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

