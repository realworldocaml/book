all: main

# Step 1
bindings.cmx: bindings.ml
	ocamlfind ocamlopt -w '@f@p@u@s@40' -package ctypes,ctypes.foreign,ctypes.stubs -c bindings.ml

# Step 2
bindings_c_gen.cmx: bindings_c_gen.ml bindings.cmx
	ocamlfind ocamlopt -w '@f@p@u@s@40' -package ctypes,ctypes.foreign,ctypes.stubs -c bindings_c_gen.ml bindings.cmx

# Step 3 compiling
bindings_c_gen: bindings_c_gen.cmx bindings.cmx
	ocamlfind ocamlopt -w '@f@p@u@s@40' -package ctypes,ctypes.foreign,ctypes.stubs -linkpkg -o bindings_c_gen bindings.cmx bindings_c_gen.cmx

# Step 3 executing
bindings_stubs_gen.c: bindings_c_gen
	./bindings_c_gen

# Step 4 compiling
bindings_stubs_gen.o: bindings_stubs_gen.c
	ocamlfind ocamlc -w '@f@p@u@s@40' -package ctypes,ctypes.foreign -c bindings_stubs_gen.c

# Step 4 compiling
bindings_stubs_gen: bindings_stubs_gen.o
	cc -o bindings_stubs_gen bindings_stubs_gen.o

# Step 5
bindings_stubs.ml: bindings_stubs_gen
	./bindings_stubs_gen > bindings_stubs.ml

# Step 6
bindings_stubs.cmx: bindings_stubs.ml
	ocamlfind ocamlopt -w '@f@p@u@s@40' -package ctypes,ctypes.foreign,ctypes.stubs -c bindings_stubs.ml

# Use in the main program
main.cmx: bindings_stubs.cmx main.ml
	ocamlfind ocamlopt -w '@f@p@u@s@40' -package ctypes,ctypes.foreign,ctypes.stubs -c main.ml bindings_stubs.cmx

main: main.cmx
	ocamlfind ocamlopt -w '@f@p@u@s@40' -package ctypes,ctypes.foreign,ctypes.stubs -linkpkg -o main bindings.cmx bindings_stubs.cmx main.cmx

clean:
	-rm *.cmx *.cmi *.c *.o bindings_c_gen bindings_stubs_gen bindings_stubs.ml main

with_ocamlbuild:
	ocamlbuild -use-ocamlfind -package ctypes,ctypes.foreign,ctypes.stubs main.native

ocamlbuild_clean:
	ocamlbuild -clean
