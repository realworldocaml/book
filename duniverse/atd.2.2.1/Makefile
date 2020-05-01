DUNE ?= dune

all:
	$(DUNE) build

test:
	$(DUNE) runtest

check: test

js:
	$(DUNE) build atdgen/bin/ag_main.bc.js

clean:
	$(DUNE) clean

all-supported-ocaml-versions:
	$(DUNE) runtest --workspace dune-workspace.dev

doc:
	cd doc && sphinx-build . _build

livedoc:
	cd doc && sphinx-autobuild . _build \
	  -p 8888 -q  --host $(shell hostname) -r '\.#.*'

package := atd
opam-release:
	dune-release distrib --skip-build --skip-lint --skip-tests -n $(package)
	dune-release publish distrib --verbose -n $(package)
	dune-release opam pkg -n $(package)
	dune-release opam submit -n $(package)

.PHONY: all test clean check doc livedoc
