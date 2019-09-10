(rule
 (target findlib.conf)
 (action (write-file %{target} "")))

(alias
 (name   runtest)
 (deps   (:x README.md)
         (:conf findlib.conf)
         (package mdx)
         (package core)
         (package ppx_jane)
         prelude.ml)
 (locks ../../global-lock)
 (action (progn
           (setenv OCAMLFIND_CONF %{conf} (run ocaml-mdx test --prelude=prelude.ml --direction=to-md %{x}))

           (diff? %{x} %{x}.corrected))))
