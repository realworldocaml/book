gcc -fPIC -Wall -I`ocamlc -where` -L`ocamlc -where` -ltermcap -lm -ldl -o final_out main.c embed_out.o -lcamlrun
./final_out
