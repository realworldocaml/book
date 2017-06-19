OCAMLC=ocamlfind c
OCAMLOPT=ocamlfind opt
FLAGS=-package ocaml-migrate-parsetree
TARGETS=topexpect_ignore topexpect_ignore.cmo topexpect_ignore.cmx topexpect_ignore.cmxs

all: build
	
clean:
	rm -f *.o *.cm* $(TARGETS)

build: $(TARGETS)

%.cmo: %.ml
	$(OCAMLC) $(FLAGS) -c $^

%.cmx: %.ml
	$(OCAMLOPT) $(FLAGS) -c $^

topexpect_ignore.cmxs: topexpect_ignore.cmx
	$(OCAMLOPT) -o $@ -shared $^

topexpect_ignore: topexpect_ignore.cmx standalone.ml
	$(OCAMLOPT) $(FLAGS) -o $@ -linkpkg $^
