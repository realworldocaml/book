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
ppx_jane \
re \
sexp_pretty \
textwrap \
yojson

# these do not exist in opam-repository yet
DUNIVERSE_SPECIFIC_DEPS = tls-lwt

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
		--pin mdx,https://github.com/realworldocaml/mdx.git,master \
		rwo \
		$(DEPS) $(DUNIVERSE_SPECIFIC_DEPS)

duniverse-upgrade: duniverse-init
	$(DUNIVERSE) pull --no-cache
