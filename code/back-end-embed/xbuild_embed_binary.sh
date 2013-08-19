gcc -fPIC -Wall -I`ocamlc -where` -L`ocamlc -where` -ltermcap -lm -ldl -o finalbc.native main.c embed_out.o -lcamlrun
./finalbc.native
