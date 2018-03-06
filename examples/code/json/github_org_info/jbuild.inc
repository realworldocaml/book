(jbuild_version 1)

(alias (
  (name cram)
  (deps (github_org.sh (files_recursively_in .)))
  (action (
    progn
    (run   cram ${<})
    (diff? ${<} ${<}.corrected)))))

