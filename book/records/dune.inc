(alias
 (name runtest)
 (deps
  (:x README.md)
  (package core)
  (package mdx)
  (package ppx_jane)
  (package re)
  prelude.ml)
 (locks ../../global-lock)
 (action
  (progn
   (run ocaml-mdx test --prelude=prelude.ml --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))

(alias
 (name runtest-all)
 (deps
  (:x README.md)
  (package core)
  (package mdx)
  (package ppx_jane)
  (package re)
  prelude.ml)
 (locks ../../global-lock)
 (action
  (progn
   (run ocaml-mdx test --prelude=prelude.ml --direction=to-md
     --non-deterministic %{x})
   (diff? %{x} %{x}.corrected))))
