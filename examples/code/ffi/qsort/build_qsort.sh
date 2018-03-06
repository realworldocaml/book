  $ jbuilder build qsort.exe
  $ cat input.txt
  2
  4
  1
  3
  $ ./_build/default/qsort.exe < input.txt
  1
  2
  3
  4
  $ corebuild -pkg ctypes.foreign qsort.inferred.mli
  ocamlfind ocamldep -package ctypes.foreign -package core -ppx 'ppx-jane -as-ppx' -modules qsort.ml > qsort.ml.depends
  ocamlfind ocamlc -i -thread -short-paths -package ctypes.foreign -package core -ppx 'ppx-jane -as-ppx' qsort.ml > qsort.inferred.mli
  $ cp _build/qsort.inferred.mli qsort.mli
