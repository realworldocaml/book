OCAMLC = ocamlc
OCAMLCFLAGS =

all: hashmap.cmo dlist.cmo lazy.cmo memo.cmo hashcons.cmo

%.cmo %.cmi: %.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c $*.ml

hashcons.cmo: hashmap.cmi
memo.cmo: hashmap.cmi
hashmap.cmo: dlist.cmi
