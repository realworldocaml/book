(alias
 (name runtest)
 (deps
  (:x README.md)
  (package core)
  (package mdx)
  (package ppx_jane)
  (:y8 examples/dictionary.ml)
  (:y7 examples/dictionary.mli)
  (:y6 examples/dictionary2.ml)
  (:y5 examples/dlist.ml)
  (:y4 examples/dlist.mli)
  (:y3 examples/let_rec.ml)
  (:y2 examples/remember_type.ml)
  (:y1 examples/time_converter/time_converter.ml)
  (:y0 examples/time_converter2.ml)
  fib.ml
  letrec.ml
  memo.ml
  prelude.ml)
 (locks ../../global-lock)
 (action
  (progn
   (run ocaml-mdx test --prelude=prelude.ml --prelude=memo:memo.ml
     --prelude=fib:fib.ml --prelude=letrec:letrec.ml
     --prelude=value_restriction:letrec.ml %{x})
   (diff? %{x} %{x}.corrected)
   (diff? %{y8} %{y8}.corrected)
   (diff? %{y7} %{y7}.corrected)
   (diff? %{y6} %{y6}.corrected)
   (diff? %{y5} %{y5}.corrected)
   (diff? %{y4} %{y4}.corrected)
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
  (:y8 examples/dictionary.ml)
  (:y7 examples/dictionary.mli)
  (:y6 examples/dictionary2.ml)
  (:y5 examples/dlist.ml)
  (:y4 examples/dlist.mli)
  (:y3 examples/let_rec.ml)
  (:y2 examples/remember_type.ml)
  (:y1 examples/time_converter/time_converter.ml)
  (:y0 examples/time_converter2.ml)
  fib.ml
  letrec.ml
  memo.ml
  prelude.ml)
 (locks ../../global-lock)
 (action
  (progn
   (run ocaml-mdx test --prelude=prelude.ml --prelude=memo:memo.ml
     --prelude=fib:fib.ml --prelude=letrec:letrec.ml
     --prelude=value_restriction:letrec.ml --non-deterministic %{x})
   (diff? %{x} %{x}.corrected)
   (diff? %{y8} %{y8}.corrected)
   (diff? %{y7} %{y7}.corrected)
   (diff? %{y6} %{y6}.corrected)
   (diff? %{y5} %{y5}.corrected)
   (diff? %{y4} %{y4}.corrected)
   (diff? %{y3} %{y3}.corrected)
   (diff? %{y2} %{y2}.corrected)
   (diff? %{y1} %{y1}.corrected)
   (diff? %{y0} %{y0}.corrected))))
