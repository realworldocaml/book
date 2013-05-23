#!/bin/sh -ex

ocamlbuild -j 4 -use-ocamlfind read_json.native
ocamlbuild -j 4 -use-ocamlfind parse.native
