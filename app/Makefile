.PHONY: all clean

OCAMLMAKEFILE = ../OCamlMakefile

all: byte-code rwo-expect

RESULT = rwo

## Producing a toplevel for Rwo_scripts

rwo-expect: rwo_expect.ml
	$(OCAMLFIND) c -g -linkpkg -linkall -predicates create_toploop \
		-package toplevel_expect_test $^ -o $@


clean::
	rm -f *.cmt *.cmti rwo-expect rwo_expect.cm*

## App description

SOURCES = app.ml

LIBS = ../lib/rwo

# Don't use INCDIRS for ../lib:
# it causes ocamldep to scan ../lib, discover dependencies erroneously
# interpreted by OCamlMakefile making the build unstable.
OCAMLFLAGS = -I ../lib

include $(OCAMLMAKEFILE)
include ../Makefile.derived
include ../Makefile.shared
