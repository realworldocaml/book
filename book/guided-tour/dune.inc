(alias
 (name runtest)
 (deps
  (:x README.md)
  (package base)
  (package mdx)
  (package ppx_jane)
  (:y1 examples/recursion.ml)
  (:y0 examples/sum/sum.ml)
  (source_tree examples/sum)
  prelude.ml)
 (locks ../../global-lock)
 (action
  (progn
   (run ocaml-mdx test --prelude=prelude.ml --direction=to-md %{x})
   (diff? %{x} %{x}.corrected)
   (diff? %{y1} %{y1}.corrected)
   (diff? %{y0} %{y0}.corrected))))
