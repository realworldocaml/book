#!/bin/sh -ex

eval `opam config env`

opam install mirage

cd mirage/example

mirage configure -t unix && make depend && mirage build && mirage clean
export BUILD=client && mirage configure -t unix && make depend && mirage build && _build/main.native && mirage clean

cd ../example2
mirage configure && make depend && mirage build && mirage clean
