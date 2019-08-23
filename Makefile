.PHONY: all clean dep publish promote test test-all docker depext

all:
	@dune build @site
	@echo Site has been generated in _build/default/static/

vendor:
	duniverse init rwo `cat pkgs` --pin mdx,https://github.com/Julow/mdx.git,duniverse_mode

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
	opam depext -y core async ppx_sexp_conv dune toplevel_expect_test patdiff \
		lambdasoup sexp_pretty fmt re mdx ctypes-foreign conf-ncurses
