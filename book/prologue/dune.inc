(alias
 (name runtest)
 (deps
  (:x README.md)
  (package mdx))
 (locks ../../global-lock)
 (action
  (progn
   (run ocaml-mdx test %{x})
   (diff? %{x} %{x}.corrected))))
