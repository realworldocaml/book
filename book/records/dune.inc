(alias
 (name   runtest)
 (deps   (:x README.md)
         (package mdx)
         prelude.ml)
 (action (progn
           (run ocaml-mdx test --prelude=prelude.ml --direction=to-ml %{x})

           (diff? %{x} %{x}.corrected))))
(alias
 (name   runtest-all)
 (deps   (:x README.md)
         (package mdx)
         prelude.ml)
 (action (progn
           (run ocaml-mdx test --prelude=prelude.ml --direction=to-ml --non-deterministic %{x})

           (diff? %{x} %{x}.corrected))))
