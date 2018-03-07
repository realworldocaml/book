### o
  $ rm -f embed_out.c
  $ ocamlc -output-obj -o embed_out.o embed_me1.ml embed_me2.ml
### c
  $ ocamlc -output-obj -o embed_out.c embed_me1.ml embed_me2.ml
