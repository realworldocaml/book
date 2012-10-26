#!/bin/sh -ex

ocamlopt unix.cmxa barrier.ml -o barrier
./barrier
