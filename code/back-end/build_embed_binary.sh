gcc -Wall -I`ocamlc -where` -L`ocamlc -where` -lcamlrun -ltermcap -o final_out embed_out.o main.c
./final_out
