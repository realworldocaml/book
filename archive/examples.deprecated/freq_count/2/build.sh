#!/usr/bin/env bash

TARGET=freq
ocamlbuild -use-ocamlfind $TARGET.byte && mv $TARGET.byte $TARGET -clib -short-paths

