.PHONY: all clean publish promote test test-all docker depext \
	duniverse-init duniverse-upgrade

DUNIVERSE ?= duniverse

all:
	@dune build @site @pdf
	@echo The site and the pdf have been generated in _build/default/static/

test:
	dune runtest

test-all:
	dune runtest --profile non-deterministic

promote:
	dune promote

clean:
	dune clean

docker:
	docker build -t ocaml/rwo .

duniverse-init:
	$(DUNIVERSE) init

duniverse-upgrade: duniverse-init
	rm -rf duniverse/
	$(DUNIVERSE) pull
