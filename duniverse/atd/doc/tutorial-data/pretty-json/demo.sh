#! /bin/sh -e

set -x
cat single.json
ydump single.json
cat stream.json
ydump -s stream.json

cat prettify.ml
ocamlfind ocamlopt -o prettify prettify.ml -package atdgen -linkpkg
./prettify
