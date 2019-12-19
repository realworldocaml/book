build:
	dune build

test:
	dune runtest

examples:
	dune build @examples

doc:
	dune build @doc

clean:
	dune clean

gh-pages: doc
	git clone `git config --get remote.origin.url` .gh-pages --reference .
	git -C .gh-pages checkout --orphan gh-pages
	git -C .gh-pages reset
	git -C .gh-pages clean -dxf
	cp -rt .gh-pages/ _build/default/_doc/_html/*
	git -C .gh-pages add .
	git -C .gh-pages commit -m "Update Pages"
	git -C .gh-pages push origin gh-pages -f
	rm -rf .gh-pages

all-supported-ocaml-versions:
	dune build @install @runtest --workspace dune-workspace.dev

.PHONY: build test doc clean examples all-supported-ocaml-versions gh-pages
