(alias
 (name runtest)
 (deps
  (:x README.md)
  (package core)
  (package mdx)
  (package ppx_jane)
  (:y4 examples/variants-termcol-annotated/dune)
  (:y3 examples/variants-termcol-annotated/terminal_color.ml)
  (:y2 examples/variants-termcol-fixed/terminal_color.ml)
  (:y1 examples/variants-termcol/terminal_color.ml)
  (:y0 examples/variants-termcol/terminal_color.mli)
  (source_tree examples/variants-termcol-annotated)
  catch_all.ml
  logger.ml
  prelude.ml)
 (locks ../../global-lock)
 (action
  (progn
   (run ocaml-mdx test --prelude=prelude.ml --prelude=catch_all:catch_all.ml
     --prelude=logger:logger.ml %{x})
   (diff? %{x} %{x}.corrected)
   (diff? %{y4} %{y4}.corrected)
   (diff? %{y3} %{y3}.corrected)
   (diff? %{y2} %{y2}.corrected)
   (diff? %{y1} %{y1}.corrected)
   (diff? %{y0} %{y0}.corrected))))
