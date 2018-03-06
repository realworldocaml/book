(jbuild_version 1)

(alias (
  (name cram)
  (deps (infer_ncurses.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

