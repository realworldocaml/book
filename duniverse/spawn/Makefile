INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

# Default rule
default:
	dune build @install

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

reinstall: uninstall reinstall

all-supported-ocaml-versions:
	dune build @install @runtest --workspace jbuild-workspace.dev

clean:
	dune clean

test:
	dune runtest

promote:
	dune promote

.PHONY: default install uninstall reinstall clean test
