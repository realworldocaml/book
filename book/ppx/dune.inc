(alias
 (name runtest)
 (deps
  (:x README.md)
  (package core)
  (package mdx)
  (package ppx_jane)
  prelude.ml)
 (locks ../../global-lock)
 (action
  (progn
   (run ocaml-mdx test --prelude=prelude.ml --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))
