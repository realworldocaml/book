(alias
 (name   runtest)
 (deps   (:x README.md)
         (:y2 ../../examples/code/objects/is_barbell.ml)
         (:y1 ../../examples/code/objects/narrowing.ml)
         (:y0 ../../examples/code/objects/subtyping.ml)
         prelude.ml
         subtyping.ml)
 (action (progn
           (run ocaml-mdx test --prelude=prelude.ml --prelude=subtyping.ml --direction=to-ml %{x})
           (diff? %{x} %{x}.corrected)
           (diff? %{y2} %{y2}.corrected)
           (diff? %{y1} %{y1}.corrected)
           (diff? %{y0} %{y0}.corrected))))
(alias
 (name   runtest-all)
 (deps   (:x README.md)
         (:y2 ../../examples/code/objects/is_barbell.ml)
         (:y1 ../../examples/code/objects/narrowing.ml)
         (:y0 ../../examples/code/objects/subtyping.ml)
         prelude.ml
         subtyping.ml)
 (action (progn
           (run ocaml-mdx test --prelude=prelude.ml --prelude=subtyping.ml --direction=to-ml --non-deterministic %{x})
           (diff? %{x} %{x}.corrected)
           (diff? %{y2} %{y2}.corrected)
           (diff? %{y1} %{y1}.corrected)
           (diff? %{y0} %{y0}.corrected))))
