.PHONY: all generate clean publish promote test test-all docker bash depext \
	duniverse-init duniverse-upgrade

DUNIVERSE ?= duniverse
HERE := $(shell pwd)

all:
	docker run --rm -it -v "$(HERE):/data" ocaml/rwo bash -c 'make -C /data generate && make -C /data test'

generate:
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

depext: docker

docker:
	docker build -t ocaml/rwo .

bash:
	docker run --rm -it -v "$(HERE):/data" ocaml/rwo bash

server:
	cohttp-server-lwt _build/default/static-wip
