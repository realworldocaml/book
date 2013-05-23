#!/usr/bin/env bash

eval `opam config -env`

echo $PATH
echo $CAML_LD_LIBRARY_PATH
echo `which ocamlbuild`

for TARGET in $*
do
  ocamlbuild -use-ocamlfind $TARGET -cflags "-w @A-4-33-41-42-43-34"
done


