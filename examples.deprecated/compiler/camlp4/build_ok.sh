#!/bin/sh
ocamlfind ocamlc -c -syntax camlp4o -package sexplib.syntax -package fieldslib.syntax type_conv_example.ml
ocamlfind ocamlc -verbose -c -syntax camlp4o -package sexplib.syntax -package fieldslib.syntax type_conv_example.ml
