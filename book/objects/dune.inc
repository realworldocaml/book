(alias
 (name runtest)
 (deps
  (:x README.md)
  (package core)
  (package mdx)
  (package ppx_jane)
  prelude.ml
  subtyping.ml)
 (locks ../../global-lock)
 (action
  (progn
   (run ocaml-mdx test --prelude=prelude.ml --prelude=subtyping.ml %{x})
   (diff? %{x} %{x}.corrected))))
