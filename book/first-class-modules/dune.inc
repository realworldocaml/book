(alias
 (name   runtest)
 (deps   (:x README.md)
         (:y2 ../../examples/code/fcm/query_handler_loader/query_handler.ml)
         (:y1 ../../examples/code/fcm/query_handler_loader/query_handler_core.ml)
         (:y0 ../../examples/code/fcm/query_handler_loader/query_handler_loader.ml)
         prelude.ml
         (package mdx))
 (action (progn
           (run mdx test --prelude=prelude.ml --direction=to-ml %{x})
           (diff? %{x} %{x}.corrected)
           (diff? %{y2} %{y2}.corrected)
           (diff? %{y1} %{y1}.corrected)
           (diff? %{y0} %{y0}.corrected))))
