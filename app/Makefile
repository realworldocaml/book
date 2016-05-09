.PHONY: all clean

OCAMLMAKEFILE = ../OCamlMakefile

all: byte-code

RESULT = rwo

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
