OCAMLC = ocamlc
OCAMLCFLAGS =

all: hashmap.cmo dlist.cmo

%.cmo: %.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c $*.ml

hashmap.cmo: dlist.cmo
