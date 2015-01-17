#!/bin/sh -ex
ocamlc -cclib -lncurses -custom -o test unix.cma curses.mli curses_stubs.c test.ml
