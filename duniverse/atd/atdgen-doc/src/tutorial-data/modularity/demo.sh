#! /bin/sh -e

set -x
cat part1.atd
cat part2.atd
cat part3.atd
for x in part1 part2 part3; do
    atdgen -t $x.atd
    atdgen -j $x.atd
    ocamlfind ocamlc -c ${x}_t.mli -package atdgen
    ocamlfind ocamlc -c ${x}_j.mli -package atdgen
    ocamlfind ocamlopt -c ${x}_t.ml -package atdgen
    ocamlfind ocamlopt -c ${x}_j.ml -package atdgen
done
ocamlfind ocamlopt -c main.ml -package atdgen

ocamlfind ocamlopt -o test_modularity \
  part1_t.cmx part1_j.cmx \
  part2_t.cmx part2_j.cmx \
  part3_t.cmx part3_j.cmx \
  main.cmx \
  -package atdgen -linkpkg
./test_modularity
