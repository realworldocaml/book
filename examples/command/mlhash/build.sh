#!/bin/sh

ocamlbuild -use-ocamlfind basic.byte basic_opt.byte basic_file.byte basic_flags.byte basic_flags.native
