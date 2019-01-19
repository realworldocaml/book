(alias
 (name   runtest)
 (deps   (:x README.md))
 (action (progn
           (run mdx test --direction=to-ml %{x})
           (diff? %{x} %{x}.corrected)
)))
