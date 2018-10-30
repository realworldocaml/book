(alias
 (name   runtest)
 (deps   (:x README.md)
         ../prelude.ml
         (package mdx))
 (action (progn
           (run mdx test --prelude=../prelude.ml --direction=infer-timestamp %{x})
           (diff? %{x} %{x}.corrected)
)))
