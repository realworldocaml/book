OCAMLMAKEFILE = ../OCamlMakefile

all: byte-code

RESULT = rwo

## Update rwo_about according to git hash

clean::
	rm -f *.cmt *.cmti

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
