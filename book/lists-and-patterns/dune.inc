(rule
 (target findlib.conf)
 (action (write-file %{target} "")))

(alias
 (name   runtest)
 (deps   (:x README.md)
         (:conf findlib.conf)
         (package mdx)
         (package core)
         (package core_bench)
         (package ppx_jane)
         prelude.ml)
 (locks ../../global-lock)
 (action (progn
           (setenv OCAMLFIND_CONF %{conf} (run ocaml-mdx test --prelude=prelude.ml --direction=to-ml %{x}))

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest-all)
 (deps   (:x README.md)
         (:conf findlib.conf)
         (package mdx)
         (package core)
         (package core_bench)
         (package ppx_jane)
         prelude.ml)
 (locks ../../global-lock)
 (action (progn
           (setenv OCAMLFIND_CONF %{conf} (run ocaml-mdx test --prelude=prelude.ml --direction=to-ml --non-deterministic %{x}))

           (diff? %{x} %{x}.corrected))))
