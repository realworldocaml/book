(alias
 (name runtest)
 (deps
  (:x README.md)
  (package core)
  (package mdx)
  (package ppx_jane)
  (:y2 examples/query_handler_loader/query_handler.ml)
  (:y1 examples/query_handler_loader/query_handler_core.ml)
  (:y0 examples/query_handler_loader/query_handler_loader.ml)
  (source_tree examples/query_handler_loader)
  prelude.ml)
 (locks ../../global-lock)
 (action
  (progn
   (run ocaml-mdx test --prelude=prelude.ml --direction=to-md %{x})
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
  (:y2 examples/query_handler_loader/query_handler.ml)
  (:y1 examples/query_handler_loader/query_handler_core.ml)
  (:y0 examples/query_handler_loader/query_handler_loader.ml)
  (source_tree examples/query_handler_loader)
  prelude.ml)
 (locks ../../global-lock)
 (action
  (progn
   (run ocaml-mdx test --prelude=prelude.ml --direction=to-md
     --non-deterministic %{x})
   (diff? %{x} %{x}.corrected)
   (diff? %{y2} %{y2}.corrected)
   (diff? %{y1} %{y1}.corrected)
   (diff? %{y0} %{y0}.corrected))))
