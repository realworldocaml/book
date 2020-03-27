.PHONY: all clean test

MENHIR          := menhir

MENHIRFLAGS     := --infer

OCAMLBUILD      := ocamlbuild -use-ocamlfind -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS)"

MAIN            := calc

all:
	$(OCAMLBUILD) $(MAIN).native

clean:
	rm -f *~ .*~
	$(OCAMLBUILD) -clean

test: all
	@echo "The following command should print 42:"
	echo "(1 + 2 * 10) * 2" | ./$(MAIN).native --algebraic
	@echo "The following command should print 42:"
	echo " 1 2 10 * + 2 *" | ./$(MAIN).native --reverse
