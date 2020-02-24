(alias
 (name runtest)
 (deps
  (:x README.md)
  (package core)
  (package mdx)
  (package ppx_jane)
  (:y4 examples/multiple_inheritance.ml)
  (:y3 examples/multiple_inheritance_wrong.ml)
  (:y2 examples/shapes/dune)
  (:y1 examples/shapes/shapes.ml)
  (:y0 examples/verbose_shapes.ml)
  (source_tree examples/shapes)
  prelude.ml)
 (locks ../../global-lock)
 (action
  (progn
   (run ocaml-mdx test --prelude=prelude.ml %{x})
   (diff? %{x} %{x}.corrected)
   (diff? %{y4} %{y4}.corrected)
   (diff? %{y3} %{y3}.corrected)
   (diff? %{y2} %{y2}.corrected)
   (diff? %{y1} %{y1}.corrected)
   (diff? %{y0} %{y0}.corrected))))
