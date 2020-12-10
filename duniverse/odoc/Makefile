.PHONY : build
build :
	dune build

.PHONY : npm-package
npm-package : npm-build
	esy release

.PHONY : npm-build
npm-build :
	esy install
	esy build

.PHONY : npm-test
npm-test :
	esy make test

.PHONY : docs
docs :
	mkdir -p docs
	dune build @doc
	cp -R _build/default/_doc/_html/* docs

.PHONY : docs
serve :
	python -m SimpleHTTPServer $(PORT)

.PHONY : test
test : build
	dune build @test/parser/runtest --no-buffer -j 1
	dune build @test/compile/runtest --no-buffer -j 1
	dune build @test/html/runtest --no-buffer -j 1

ODOC_RELATIVE_PATH := ../../_build/install/default/bin/

.PHONY : dune-test
dune-test : build
	(cd test/dune && PATH=$(ODOC_RELATIVE_PATH):$$PATH dune build @doc)

COVERAGE := _coverage
BISECT_FILES_PATTERN := _build/default/test/*/bisect*.out

.PHONY : coverage
coverage :
	find . -name 'bisect*.out' | xargs rm -f
	BISECT_ENABLE=yes dune build @test/parser/runtest --no-buffer -j 1 --force
	BISECT_ENABLE=yes dune build @test/html/runtest --no-buffer -j 1 --force
	@bisect-ppx-report \
	    -I _build/default/ -html $(COVERAGE)/ \
	    -text - -summary-only \
	    $(BISECT_FILES_PATTERN)
	@echo See $(COVERAGE)/index.html

.PHONY : clean
clean :
	dune clean
	(cd test/dune && dune clean)
	rm -rf $(COVERAGE)
