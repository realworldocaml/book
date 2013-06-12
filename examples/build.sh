#!/usr/bin/env bash

eval `opam config -env`

echo $PATH
echo $CAML_LD_LIBRARY_PATH
echo `which ocamlbuild`

ocamlbuild -j 4 -use-ocamlfind $TARGET -cflags "-w @A-4-33-41-42-43-34-44" $*



