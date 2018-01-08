OCAMLMAKEFILE = ../OCamlMakefile

all: byte-code-library

RESULT = rwo

## Update rwo_about according to git hash

rwo_about.ml: rwo_about.ml.m4 ../Makefile.derived
	m4 -D 'GIT_COMMIT=$(GIT_COMMIT)' $< > $@

clean::
	rm -f rwo_about.ml

PRE_TARGETS += rwo_about.ml

## Library description

SOURCES =                                \
  rwo_about.ml                           \
  rwo_bash_script.mli rwo_bash_script.ml \
  rwo_util.mli        rwo_util.ml        \
  rwo_core2.mli       rwo_core2.ml       \
  rwo_html.mli        rwo_html.ml        \
  rwo_index.mli       rwo_index.ml       \
  rwo_lang.mli        rwo_lang.ml        \
  rwo_import.mli      rwo_import.ml      \
  rwo_references.mli  rwo_references.ml      \
  rwo_ppre.mli        rwo_ppre.ml        \
  rwo_pygments.mli    rwo_pygments.ml    \
  rwo_expect.mli      rwo_expect.ml      \
  rwo_scripts.mli     rwo_scripts.ml     \
  rwo_toc.mli         rwo_toc.ml         \
  rwo_book.mli        rwo_book.ml        \
  rwo.ml

include $(OCAMLMAKEFILE)
include ../Makefile.derived
include ../Makefile.shared
