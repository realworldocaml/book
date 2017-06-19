OCAMLC=ocamlfind c
OCAMLOPT=ocamlfind opt
FLAGS=-package ocaml-migrate-parsetree
TARGETS= \
	topexpect_ignore topexpect_ignore.cmo \
	topexpect_ignore.cmx topexpect_ignore.cmxs \
	merlin_topexpect_ignore merlin_topexpect_ignore.cmo \
	merlin_topexpect_ignore.cmx merlin_topexpect_ignore.cmxs

all: build
	
clean:
	rm -f *.o *.cm* $(TARGETS)

build: $(TARGETS)

%.cmo: %.ml
	$(OCAMLC) $(FLAGS) -c $^

%.cmx: %.ml
	$(OCAMLOPT) $(FLAGS) -c $^

%.cmxs: %.cmx
	$(OCAMLOPT) -o $@ -shared $^

topexpect_ignore merlin_topexpect_ignore:
	$(OCAMLOPT) $(FLAGS) -o $@ -linkpkg $^

topexpect_ignore: topexpect_ignore.cmx standalone.ml
merlin_topexpect_ignore: merlin_topexpect_ignore.cmx standalone.ml
