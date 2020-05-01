PWD := $(shell pwd)

COQINCLUDE := -R $(PWD) MenhirLib \

export COQINCLUDE

.PHONY: all clean install uninstall

all: _CoqProject
	@ $(MAKE) -f Makefile.coq --no-print-directory all

_CoqProject:
	@ $(MAKE) -f Makefile.coq --no-print-directory _CoqProject

clean:
	@ $(MAKE) -f Makefile.coq --no-print-directory clean
	@ rm -f _CoqProject

# The role of DESTDIR is explained here:
# https://www.gnu.org/prep/standards/html_node/DESTDIR.html
# Basically, it is empty in a normal installation.
# A nonempty value can be used to perform a dummy installation
# in a different location.

CONTRIB = $(shell $(COQBIN)coqc -where)/user-contrib
TARGET  = $(DESTDIR)$(CONTRIB)/MenhirLib

install: all
	rm -rf $(TARGET)
	mkdir -p $(TARGET)
	install -m 644 *.v *.vo *.glob $(TARGET)

uninstall:
	rm -rf $(TARGET)
