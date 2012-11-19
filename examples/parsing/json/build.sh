#!/bin/sh -ex

ocamlbuild -j 4 -use-ocamlfind parse.native
