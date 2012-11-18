#!/usr/bin/env bash

eval `opam config -env`

ocamlbuild -use-ocamlfind search.native
