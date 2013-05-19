#!/bin/sh -ex

ocamlfind ocamlopt -linkpkg -package ctypes -package unix -cclib -lncurses ncurses.ml hello.ml -o hello
