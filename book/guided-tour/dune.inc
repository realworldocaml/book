(alias
 (name   runtest)
 (deps   (:x README.md)
         (:y1 ../../examples/code/guided-tour/recursion.ml)
         (:y0 ../../examples/code/guided-tour/sum/sum.ml)
         (source_tree ../../examples/code/guided-tour/sum)
         prelude.ml)
 (action (progn
           (run mdx test --prelude=prelude.ml --direction=to-ml %{x})
           (diff? %{x} %{x}.corrected)
           (diff? %{y1} %{y1}.corrected)
           (diff? %{y0} %{y0}.corrected))))
