.PHONY: all
all: prepare
# These preparations can be performed in parallel.
	dune build @executables @sentences
# The benchmark itself must be run sequentially.
	dune build @benchmark --force -j 1

.PHONY: prepare
prepare:
# Recreate the directory build/.
	rm -rf build &&	mkdir -p build
# Copy template/ into it.
	cp -r template build/
	mkdir -p build/template/sentences
# Create the backends from the files template/backends/*.flags.
	make -C build/template/backends
# Create the files build/template/dune and build/template/sentences/dune.
	cd build/template && ocaml dune.ml
# Replicate these files for every parser.
	for XXX in $$(cd parsers && ls) ; do \
	  cp -r build/template build/"$$XXX" && \
	  cp -r parsers/"$$XXX"/* build/"$$XXX"/src ; \
	done
	rm -rf build/template

.PHONY: clean
clean:
	rm -rf build
