#!/bin/sh -ex

mkdir -p _build
opam config -env > _build/env.sh
. ./_build/env.sh
ocamlbuild -j 4 -use-ocamlfind rwoserver.native rwocli.native downtime.native redirect.native rwoserver_cache.native
