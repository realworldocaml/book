OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLRUN=ocamlrun

include $(shell $(OCAMLC) -where)/Makefile.config

CAMLCFLAGS=
CAMLOPTFLAGS=$(CAMLCFLAGS)

FILES=test.ml test_nats.ml test_big_ints.ml test_ratios.ml test_nums.ml test_io.ml end_test.ml

all:: test.byt
	@echo "----- Testing in bytecode..."
	$(OCAMLRUN) -I ../src ./test.byt

ifneq "$(ARCH)" "none"
all:: test.exe
	@echo "----- Testing in native code..."
	./test.exe
endif

test.byt: $(FILES) ../src/nums.cma ../src/libnums.$(A)
	$(OCAMLC) -I ../src $(CAMLCFLAGS) ../src/nums.cma $(FILES) -o test.byt

test.exe: $(FILES) ../src/nums.cmxa ../src/libnums.$(A)
	$(OCAMLOPT) -I ../src $(CAMLOPTFLAGS) ../src/nums.cmxa $(FILES) -o test.exe

%.cmi: %.mli
	$(OCAMLC) $(CAMLCFLAGS) -c $*.mli
%.cmo: %.ml
	$(OCAMLC) $(CAMLCFLAGS) -c $*.ml
%.cmx: %.ml
	$(OCAMLOPT) $(CAMLOPTFLAGS) -c $*.ml

clean:
	rm -f *.cm[ioxt] *.$(O) test.byt test.exe
