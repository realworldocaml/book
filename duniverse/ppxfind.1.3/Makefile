INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

.PHONY: all
all:
	dune build @install

.PHONY: install
install:
	dune install $(INSTALL_ARGS)

.PHONY: uninstall
uninstall:
	dune uninstall $(INSTALL_ARGS)

.PHONY: reinstall
reinstall:
	$(MAKE) uninstall
	$(MAKE) install

.PHONY: test
test:
	dune runtest

.PHONY: clean
clean:
	dune clean

.PHONY: all-supported-ocaml-versions
all-supported-ocaml-versions:
	dune build --workspace jbuild-workspace.dev
