#!/bin/sh -ex

#ocamlfind ocamlc -i -package ctypes -package unix -cclib -lncurses ncurses.ml > ncurses.mli
ocamlfind ocamlopt -linkpkg -package ctypes -package unix -cclib -lncurses ncurses.mli ncurses.ml hello.ml -o hello
ocamlfind ocamlopt -linkpkg -package ctypes -package unix ncurses.mli ncurses.ml hello.ml -o hello_broken
