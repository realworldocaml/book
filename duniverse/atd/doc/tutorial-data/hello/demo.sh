#! /bin/sh -e

set -x
cat hello.atd
atdgen -t hello.atd
atdgen -j hello.atd
ls
ocamlfind ocamlc -c hello_t.mli -package atdgen
ocamlfind ocamlc -c hello_j.mli -package atdgen
ocamlfind ocamlopt -c hello_t.ml -package atdgen
ocamlfind ocamlopt -c hello_j.ml -package atdgen
ocamlfind ocamlopt -c hello.ml -package atdgen
ocamlfind ocamlopt -o hello hello_t.cmx hello_j.cmx hello.cmx \
  -package atdgen -linkpkg
./hello
