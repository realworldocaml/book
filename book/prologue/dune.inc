(alias
 (name runtest)
 (deps
  (:x README.md)
  (package mdx))
 (locks ../../global-lock)
 (action
  (progn
   (run ocaml-mdx test --direction=to-md %{x})
   (diff? %{x} %{x}.corrected))))
