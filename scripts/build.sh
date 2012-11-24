#!/bin/sh -ex

eval `opam config -env`
ocamlbuild -j 4 -use-ocamlfind rewrite_link_to_xref.native
