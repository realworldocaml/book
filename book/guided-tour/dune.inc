(alias
 (name runtest)
 (deps
  (:x README.md)
  (package base)
  (package mdx)
  (package ppx_jane)
  (:y2 examples/recursion.ml)
  (:y1 examples/sum/dune)
  (:y0 examples/sum/sum.ml)
  (source_tree examples/sum)
  prelude.ml)
 (locks ../../global-lock)
 (action
  (progn
   (run ocaml-mdx test --prelude=prelude.ml %{x})
   (diff? %{x} %{x}.corrected)
   (diff? %{y2} %{y2}.corrected)
   (diff? %{y1} %{y1}.corrected)
   (diff? %{y0} %{y0}.corrected))))

(alias
 (name runtest-all)
 (deps
  (:x README.md)
  (package base)
  (package mdx)
  (package ppx_jane)
  (:y2 examples/recursion.ml)
  (:y1 examples/sum/dune)
  (:y0 examples/sum/sum.ml)
  (source_tree examples/sum)
  prelude.ml)
 (locks ../../global-lock)
 (action
  (progn
   (run ocaml-mdx test --prelude=prelude.ml --non-deterministic %{x})
   (diff? %{x} %{x}.corrected)
   (diff? %{y2} %{y2}.corrected)
   (diff? %{y1} %{y1}.corrected)
   (diff? %{y0} %{y0}.corrected))))
