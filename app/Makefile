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

# Don't use LIBS...
# The rebuilding behavior annoys more than it helps
# LIBS = ../lib/rwo
OCAMLLDFLAGS += ../lib/rwo.cma
INCDIRS = ../lib

include $(OCAMLMAKEFILE)
include ../Makefile.derived
include ../Makefile.shared
