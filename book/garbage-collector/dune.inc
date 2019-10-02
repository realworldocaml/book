(alias
 (name runtest)
 (deps
  (:x README.md)
  (package async)
  (package core)
  (package core_bench)
  (package mdx)
  (package ppx_jane)
  (:y1 examples/barrier_bench/barrier_bench.ml)
  (:y0 examples/finalizer/finalizer.ml)
  (source_tree examples/barrier_bench)
  (source_tree examples/finalizer)
  prelude.ml)
 (locks ../../global-lock)
 (action
  (progn
   (run ocaml-mdx test --prelude=prelude.ml --direction=to-md %{x})
   (diff? %{x} %{x}.corrected)
   (diff? %{y1} %{y1}.corrected)
   (diff? %{y0} %{y0}.corrected))))

(alias
 (name runtest-all)
 (deps
  (:x README.md)
  (package async)
  (package core)
  (package core_bench)
  (package mdx)
  (package ppx_jane)
  (:y1 examples/barrier_bench/barrier_bench.ml)
  (:y0 examples/finalizer/finalizer.ml)
  (source_tree examples/barrier_bench)
  (source_tree examples/finalizer)
  prelude.ml)
 (locks ../../global-lock)
 (action
  (progn
   (run ocaml-mdx test --prelude=prelude.ml --direction=to-md
     --non-deterministic %{x})
   (diff? %{x} %{x}.corrected)
   (diff? %{y1} %{y1}.corrected)
   (diff? %{y0} %{y0}.corrected))))
