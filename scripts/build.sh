#!/bin/sh -ex

mkdir -p _build
opam config -env > _build/env.sh
. ./_build/env.sh
ocamlbuild -j 4 -use-ocamlfind filter_book.native html_code_highlight.native
