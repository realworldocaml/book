#!/usr/bin/env bash

TARGET=$1
ocamlbuild -use-ocamlfind $TARGET.byte && mv $TARGET.byte $TARGET


