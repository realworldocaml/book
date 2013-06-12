#!/bin/sh
ocamlc -output-obj -o embed_out.o embed_me1.ml embed_me2.ml
gcc -Wall -I `ocamlc -where` -L `ocamlc -where` -lcamlrun -ltermcap \
  -o final_out embed_out.o main.c
ocamlc -o final_out2 embed_out.o main.c
ocamlopt -output-obj -o embed_native.o embed_me1.ml embed_me2.ml
gcc -Wall -I `ocamlc -where` -L `ocamlc -where` -lasmrun -ltermcap \
  -o final_out_native embed_native.o main.c
./final_out_native

