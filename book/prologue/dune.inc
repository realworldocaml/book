(alias
 (name   runtest)
 (deps   (:x README.md)
         (package mdx))
 (action (progn
           (run ocaml-mdx test --direction=to-ml %{x})

           (diff? %{x} %{x}.corrected))))
