#!/bin/sh -ex

ocamlbuild -j 4 -use-ocamlfind read.native
ocamlbuild -j 4 -use-ocamlfind read_tree.native
ocamlbuild -j 4 -use-ocamlfind write.native
