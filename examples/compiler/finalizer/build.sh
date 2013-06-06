#!/bin/sh

ocamlfind ocamlopt -package core -package async -thread -o finalizer -linkpkg finalizer.ml
ocamlfind ocamlopt -package unix -o stdlib_finalizer -linkpkg stdlib_finalizer.ml
