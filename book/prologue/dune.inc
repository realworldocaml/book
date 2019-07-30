(rule
 (target findlib.conf)
 (action (write-file %{target} "path=\"%{project_root}/_build/install/%{context_name}/lib\"")))

(alias
 (name   runtest)
 (deps   (:x README.md)
         (:conf findlib.conf)
         (package mdx))
 (action (progn
           (setenv OCAMLFIND_CONF %{conf} (run ocaml-mdx test --direction=to-ml %{x}))

           (diff? %{x} %{x}.corrected))))
