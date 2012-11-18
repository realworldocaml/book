#!/usr/bin/env bash

eval `opam config -env`

ocamlbuild -use-ocamlfind broker.native client.native


