(alias
 (name runtest)
 (deps
  (:x README.md)
  (package core)
  (package mdx)
  (package ppx_jane)
  (:y2 examples/is_barbell.ml)
  (:y1 examples/narrowing.ml)
  (:y0 examples/subtyping.ml)
  prelude.ml
  subtyping.ml)
 (locks ../../global-lock)
 (action
  (progn
   (run ocaml-mdx test --prelude=prelude.ml --prelude=subtyping.ml
     --direction=to-md %{x})
   (diff? %{x} %{x}.corrected)
   (diff? %{y2} %{y2}.corrected)
   (diff? %{y1} %{y1}.corrected)
   (diff? %{y0} %{y0}.corrected))))

(alias
 (name runtest-all)
 (deps
  (:x README.md)
  (package core)
  (package mdx)
  (package ppx_jane)
  (:y2 examples/is_barbell.ml)
  (:y1 examples/narrowing.ml)
  (:y0 examples/subtyping.ml)
  prelude.ml
  subtyping.ml)
 (locks ../../global-lock)
 (action
  (progn
   (run ocaml-mdx test --prelude=prelude.ml --prelude=subtyping.ml
     --direction=to-md --non-deterministic %{x})
   (diff? %{x} %{x}.corrected)
   (diff? %{y2} %{y2}.corrected)
   (diff? %{y1} %{y1}.corrected)
   (diff? %{y0} %{y0}.corrected))))
