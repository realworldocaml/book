.PHONY: all clean dep publish promote test test-all docker depext \
	duniverse-init duniverse-upgrade

DUNIVERSE ?= duniverse

DEPS =\
async \
atdgen \
base \
cmdliner \
cohttp-async \
conf-ncurses \
core \
core_bench \
ctypes \
ctypes-foreign \
fmt \
lambdasoup \
mdx \
ocaml-compiler-libs \
ocaml-print-intf \
ppx_jane \
re \
sexp_pretty \
textwrap \
tls \
yojson

all:
	@dune build @site @pdf
	@echo The site and the pdf have been generated in _build/default/static/

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
	$(DUNIVERSE) init \
		--pull-mode source \
		--pin mdx,https://github.com/realworldocaml/mdx.git,master \
		rwo \
		$(DEPS) $(DUNIVERSE_SPECIFIC_DEPS)

duniverse-upgrade: duniverse-init
	rm -rf duniverse/
	$(DUNIVERSE) pull --no-cache
