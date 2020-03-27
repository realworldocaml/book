.PHONY: all clean test

MENHIR          := menhir

# We use the table back-end, and link against menhirLib.
# We assume that menhirLib has been installed in such a
# way that ocamlfind knows about it.

MENHIRFLAGS     := --table
# -- infer is automatically added by ocamlbuild.

OCAMLBUILD      := ocamlbuild -use-ocamlfind -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS)" -package menhirLib

MAIN            := calc

all:
	$(OCAMLBUILD) $(MAIN).native

clean:
	rm -f *~ .*~
	$(OCAMLBUILD) -clean

test: all
	@echo "The following command should print 42:"
	echo "(1 + 2 * 10) * 2" | ./$(MAIN).native
