.PHONY: all clean dep publish promote test test-all docker depext \
	duniverse-init duniverse-update

DEPS =\
async \
base \
cmdliner \
cohttp-async \
core \
core_bench \
ctypes \
ctypes-foreign \
fmt \
lambdasoup \
mdx \
ocaml-compiler-libs \
ppx_jane \
re \
sexp_pretty \
textwrap \
yojson

all:
	@dune build @site @pdf
	@echo The site and the pdf have been generated in _build/default/static/

vendor:
	duniverse init rwo `cat book-pkgs` --pin mdx,https://github.com/Julow/mdx.git,duniverse_mode

test:
	dune runtest

test-all:
	dune build @runtest-all

dep:
	dune exec -- rwo-dep

promote:
	dune promote

clean:
	dune clean

docker:
	docker build -t ocaml/rwo .

depext:
	opam depext -y $(DEPS)

duniverse-init:
	duniverse init \
		--pin mdx,https://github.com/Julow/mdx.git,duniverse_mode \
		$(DEPS)

duniverse-upgrade: duniverse-init
	duniverse pull --no-cache
