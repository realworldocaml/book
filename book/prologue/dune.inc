(rule
 (target findlib.conf)
 (action (write-file %{target} "")))

(alias
 (name   runtest)
 (deps   (:x README.md)
         (:conf findlib.conf)
         (package mdx))
 (locks ../../global-lock)
 (action (progn
           (setenv OCAMLFIND_CONF %{conf} (run ocaml-mdx test --direction=to-ml %{x}))

           (diff? %{x} %{x}.corrected))))
