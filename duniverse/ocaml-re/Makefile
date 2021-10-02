DUNE ?= dune

all:
	@$(DUNE) build

test:
	@$(DUNE) runtest

check: test

clean:
	@$(DUNE) clean

.PHONY: check test all clean

.PHONY: all-supported-ocaml-versions
all-supported-ocaml-versions:
	dune build @runtest --workspace dune-workspace.dev

.PHONY: release
release: ## Release on Opam
	dune-release distrib --skip-build --skip-lint --skip-tests
	dune-release publish distrib --verbose
	dune-release opam pkg
	dune-release opam submit

.PHONY: nix
nix:
	nix-shell -A resolve default.nix
