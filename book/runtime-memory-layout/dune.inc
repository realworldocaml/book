(alias
 (name   runtest)
 (deps   (:x README.md)
         prelude.ml
         (package mdx))
 (action (progn
           (run mdx test --prelude=prelude.ml --direction=to-ml %{x})
           (diff? %{x} %{x}.corrected)
)))
