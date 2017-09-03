.PHONY: all clean

EXTS=topscript rawscript sh errsh rawsh ml mli syntax c h scm S json java cpp mll mly atd cmd
ALL := $(shell find code -name jbuild $(EXTS:%=-o -name '*.%'))

all: $(ALL:%=%.sexp)
	@ :

clean:
	rm -f `find . -name '*.sexp'`

%.sexp: %
	rwo-build eval $< > $@

%.c.sexp: %.c
	rwo-build eval $< > $@
