#! /bin/sh -e

set -x

cat tree.atd
cat tree.ml

atdgen -t tree.atd
atdgen -b tree.atd
ocamlfind ocamlopt -o tree \
  tree_t.mli tree_t.ml tree_b.mli tree_b.ml tree.ml \
  -package atdgen -linkpkg
./tree

ls -l tree.dat
bdump tree.dat
bdump -w Empty,Node tree.dat
bdump tree.dat
