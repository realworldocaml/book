#ocamlfind ocamlopt -o datetime -package core -package ctypes.foreign -thread -linkpkg datetime.ml
ocamlfind ocamlopt -short-paths -annot -o qsort -package core -package ctypes.foreign -thread -linkpkg qsort.ml
ocamlfind ocamlopt -short-paths -i -package core -package ctypes.foreign -thread qsort.ml
