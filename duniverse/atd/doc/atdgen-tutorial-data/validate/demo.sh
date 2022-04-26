#! /bin/sh -e

set -x
cat resume.atd
atdgen -t resume.atd
atdgen -j resume.atd
atdgen -v resume.atd
ls
ocamlfind ocamlc -c resume_t.mli -package atdgen
ocamlfind ocamlc -c resume_v.mli -package atdgen
ocamlfind ocamlc -c resume_j.mli -package atdgen
ocamlfind ocamlopt -c resume_t.ml -package atdgen
ocamlfind ocamlopt -c resume_util.ml -package atdgen
ocamlfind ocamlopt -c resume_v.ml -package atdgen
ocamlfind ocamlopt -c resume_j.ml -package atdgen
ocamlfind ocamlopt -c resume.ml -package atdgen
ocamlfind ocamlopt -o test_resume \
  resume_t.cmx resume_util.cmx resume_v.cmx resume_j.cmx resume.cmx \
  -package atdgen -linkpkg
./test_resume
