#
# Makefile for developer's convenience.
# Build logic is implemented with dune.
#

DUNE ?= dune

# Build everything. Should only require OCaml libraries and tools installable
# via opam.
.PHONY: all
all:
	$(MAKE) -C atdpy clean-for-dune
	$(MAKE) -C atdts clean-for-dune
	$(DUNE) build

# Install the OCaml dependencies for the build.
.PHONY: setup
setup:
	opam update
	./scripts/install-opam-dependencies

# Build and test everything in a Docker container, producing an
# image named 'atd'.
# This is split into two steps because installing the dependencies takes
# forever each time.
.PHONY: docker
docker:
	$(MAKE) docker-deps
	$(MAKE) docker-build

# This takes a while and has nothing to do with atd.
.PHONY: docker-deps
docker-deps:
	docker build -t atd-deps -f dockerfiles/atd-deps.dockerfile .

.PHONY: docker-build
docker-build:
	docker build -t atd -f dockerfiles/atd.dockerfile .

############################# Testing #####################################

# Test everything. Requires external non-OCaml compilers and libraries
# to support all the target languages.
.PHONY: test
test:
	$(MAKE) test-ts
	$(MAKE) test-ocaml
	$(MAKE) test-scala
	$(MAKE) test-java
	$(MAKE) test-python
	$(MAKE) test-ts

# Test the OCaml code used by all the backends
test-common:
	$(MAKE) -C atd test
	$(MAKE) -C atdcat test

# Test only the OCaml backends
.PHONY: test-ocaml
test-ocaml:
	$(MAKE) test-common
	$(MAKE) -C atdgen-runtime test
	$(MAKE) -C atdgen test

# Test only the Scala backend
.PHONY: test-scala
test-scala:
	$(MAKE) test-common
	$(MAKE) -C atds test

# Test only the Java backend
.PHONY: test-java
test-java:
	$(MAKE) test-common
	$(MAKE) -C atdj test

# Test only the Python backend
.PHONY: test-python
test-python:
	$(MAKE) test-common
	$(MAKE) -C atdpy test

# Test only the TypeScript backend
.PHONY: test-ts
test-ts:
	$(MAKE) test-common
	$(MAKE) -C atdts test

############################################################################

.PHONY: js
js:
	$(DUNE) build atdgen/bin/ag_main.bc.js

.PHONY: clean
clean:
	$(DUNE) clean
	$(MAKE) -C atdpy clean
	rm -rf tmp

.PHONY: all-supported-ocaml-versions
all-supported-ocaml-versions:
	$(DUNE) runtest --workspace dune-workspace.dev

.PHONY: doc
doc:
	cd doc && sphinx-build . _build

# Run documentation server.
# See setup instructions in CONTRIBUTING.md
.PHONY: livedoc
livedoc:
	$(MAKE) doc
	python3 -m http.server 8888 --directory doc/_build

# Prepare the opam files for a release. They're derived from 'dune-project'
# and from the template specified in the root 'dune' file..
#
.PHONY: opam-files
opam-files:
	$(DUNE) build *.opam

# This is only part of the release process.
# See complete release instructions in CONTRIBUTING.md.
#
.PHONY: opam-release
opam-release:
	dune-release tag
	dune-release distrib
	dune-release publish
	dune-release opam pkg
	dune-release opam submit
