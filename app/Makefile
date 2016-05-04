OCAMLMAKEFILE = ../OCamlMakefile

all: byte-code

RESULT = rwo

## Update rwo_about according to git hash

clean::
	rm -f *.cmt *.cmti

## App description

SOURCES = app.ml

LIBS = ../lib/rwo
INCDIRS = ../lib

include $(OCAMLMAKEFILE)
include ../Makefile.derived
include ../Makefile.shared
