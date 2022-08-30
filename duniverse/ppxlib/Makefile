INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

# Default rule
default:
	dune build --auto-promote @install

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

reinstall: uninstall reinstall

test:
	dune runtest

doc:
	dune build @doc

clean:
	dune clean

all-supported-ocaml-versions:
	dune build @install --workspace dune-workspace.dev --root .

opam-release:
	dune-release distrib --skip-build --skip-lint --skip-tests
	dune-release publish distrib --verbose
	dune-release opam pkg
	dune-release opam submit

.PHONY: default install uninstall reinstall clean test doc
.PHONY: all-supported-ocaml-versions opam-release
