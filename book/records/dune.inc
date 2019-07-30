(rule
 (target findlib.conf)
 (action (write-file %{target} "path=\"%{project_root}/_build/install/%{context_name}/lib\"")))

(alias
 (name   runtest)
 (deps   (:x README.md)
         (:conf findlib.conf)
         (package mdx)
         (package core)
         (package ppx_jane)
         (package re)
         prelude.ml)
 (action (progn
           (setenv OCAMLFIND_CONF %{conf} (run ocaml-mdx test --prelude=prelude.ml --direction=to-ml %{x}))

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest-all)
 (deps   (:x README.md)
         (:conf findlib.conf)
         (package mdx)
         (package core)
         (package ppx_jane)
         (package re)
         prelude.ml)
 (action (progn
           (setenv OCAMLFIND_CONF %{conf} (run ocaml-mdx test --prelude=prelude.ml --direction=to-ml --non-deterministic %{x}))

           (diff? %{x} %{x}.corrected))))
