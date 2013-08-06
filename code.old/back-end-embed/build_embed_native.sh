ocamlopt -output-obj -o embed_native.o embed_me1.ml embed_me2.ml
gcc -Wall -I `ocamlc -where` -o final_out.native embed_native.o main.c -L `ocamlc -where` -lasmrun -ltermcap -lm -ldl
./final_out.native
