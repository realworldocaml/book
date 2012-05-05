#!/usr/bin/env bash

for TARGET in $*
do
  ocamlbuild -use-ocamlfind $TARGET
done


