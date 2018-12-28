(alias
 (name   runtest)
 (deps   (:x README.md)
         prelude.ml)
 (action (progn
           (run mdx test --prelude=prelude.ml --direction=to-ml %{x})
	   )))
