JBUILDER ?= jbuilder

all:
	$(JBUILDER) build

tests:
	$(JBUILDER) runtest

check: tests

clean:
	$(JBUILDER) clean

all-supported-ocaml-versions:
	$(JBUILDER) runtest --dev --workspace jbuild-workspace.dev

doc:
	cd doc && sphinx-build . _build

livedoc:
	cd doc && sphinx-autobuild . _build \
	  -p 8888 -q  --host $(shell hostname) -r '\.#.*'

.PHONY: all tests clean check doc livedoc
