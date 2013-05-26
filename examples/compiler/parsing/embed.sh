#!/bin/sh
ocamlc -output-obj -o embed_out.o embed_me1.ml embed_me2.ml
ocamlc -output-obj -o embed_out_source.c embed_me1.ml embed_me2.ml
gcc -Wall -I `ocamlc -where` -L `ocamlc -where` -lcamlrun -ltermcap \
  -o final_out embed_out.o main.c
ocamlc -o final_out2 embed_out.o main.c
