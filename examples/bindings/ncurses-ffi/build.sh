#!/bin/sh -ex

#ocamlfind ocamlc -i -package ctypes -package unix -cclib -lncurses ncurses.ml > ncurses.mli
ocamlfind ocamlopt -linkpkg -package ctypes.foreign -cclib -lncurses ncurses.mli ncurses.ml hello.ml -o hello
ocamlfind ocamlopt -bin-annot -c -package ctypes.foreign ncurses.mli ncurses.ml
#ocamlfind ocamlopt -linkpkg -package ctypes.foreign ncurses.mli ncurses.ml hello.ml -o hello_broken
