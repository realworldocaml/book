#!/bin/sh -ex

ocamlbuild -j 4 -use-ocamlfind read.native
ocamlbuild -j 4 -use-ocamlfind read_tree.native
ocamlbuild -j 4 -classic-display -use-ocamlfind write.native
ocamlbuild -j 4 -classic-display -use-ocamlfind author.native
