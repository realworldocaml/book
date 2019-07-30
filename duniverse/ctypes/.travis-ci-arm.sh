#!/bin/bash
cd $(dirname $0)
eval $(opam config env)
export OPAMYES=1
export OPAMVERBOSE=1
git clone git://github.com/ocaml/dune
cd dune && make && sudo make install
dune build
dune runtest
