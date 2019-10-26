(alias
 (name runtest)
 (deps
  (:x README.md)
  (package core)
  (package mdx)
  (package ppx_jane)
  (:y3 examples/freq-fast/counter.ml)
  (:y2 examples/freq-fast/counter.mli)
  (:y1 examples/map_vs_hash/map_vs_hash.ml)
  (:y0 examples/map_vs_hash2/map_vs_hash2.ml)
  (source_tree examples/map_vs_hash)
  (source_tree examples/map_vs_hash2)
  prelude.ml)
 (locks ../../global-lock)
 (action
  (progn
   (run ocaml-mdx test --prelude=prelude.ml --direction=to-md %{x})
   (diff? %{x} %{x}.corrected)
   (diff? %{y3} %{y3}.corrected)
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
  (:y3 examples/freq-fast/counter.ml)
  (:y2 examples/freq-fast/counter.mli)
  (:y1 examples/map_vs_hash/map_vs_hash.ml)
  (:y0 examples/map_vs_hash2/map_vs_hash2.ml)
  (source_tree examples/map_vs_hash)
  (source_tree examples/map_vs_hash2)
  prelude.ml)
 (locks ../../global-lock)
 (action
  (progn
   (run ocaml-mdx test --prelude=prelude.ml --direction=to-md
     --non-deterministic %{x})
   (diff? %{x} %{x}.corrected)
   (diff? %{y3} %{y3}.corrected)
   (diff? %{y2} %{y2}.corrected)
   (diff? %{y1} %{y1}.corrected)
   (diff? %{y0} %{y0}.corrected))))
