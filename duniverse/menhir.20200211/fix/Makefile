# ------------------------------------------------------------------------------

# The name of the library.
THIS     := fix

# The version number is automatically set to the current date,
# unless DATE is defined on the command line.
DATE     := $(shell /bin/date +%Y%m%d)

# The repository URL (https).
REPO     := https://gitlab.inria.fr/fpottier/$(THIS)

# The archive URL (https).
ARCHIVE  := $(REPO)/repository/$(DATE)/archive.tar.gz

# ------------------------------------------------------------------------------

.PHONY: all
all:
	dune build -p $(THIS)

# [make test] runs all tests.

# Some tests require the following opam packages:
#   regenerate

.PHONY: test
test:
	dune build @runtest

# [make versions] compiles Fix under many versions of OCaml, whose
# list is specified in the file dune-workspace.versions.

# This requires appropriate opam switches to exist. A missing switch
# can be created like this:
#   opam switch create 4.03.0

.PHONY: versions
versions:
	@ dune build --workspace dune-workspace.versions -p $(THIS)

.PHONY: install
install: all
	dune install -p $(THIS)

.PHONY: clean
clean:
	rm -f *~ src/*~
	dune clean

.PHONY: uninstall
uninstall:
	ocamlfind remove $(THIS) || true

.PHONY: reinstall
reinstall: uninstall
	@ make install

.PHONY: show
show: reinstall
	@ echo "#require \"fix\";;\n#show Fix;;" | ocaml

.PHONY: pin
pin:
	opam pin add $(THIS) .

.PHONY: unpin
unpin:
	opam pin remove $(THIS)

HEADACHE := headache
LIBHEAD  := $(shell pwd)/headers/library-header
FIND     := $(shell if command -v gfind >/dev/null ; then echo gfind ; else echo find ; fi)

.PHONY: headache
headache:
	@ $(FIND) src -regex ".*\.ml\(i\|y\|l\)?" \
	    -exec $(HEADACHE) -h $(LIBHEAD) "{}" ";"

.PHONY: release
release:
# Make sure the current version can be compiled and installed.
	@ make uninstall
	@ make clean
	@ make install
# Check the current package description.
	@ opam lint
# Check if everything has been committed.
	@ if [ -n "$$(git status --porcelain)" ] ; then \
	    echo "Error: there remain uncommitted changes." ; \
	    git status ; \
	    exit 1 ; \
	  else \
	    echo "Now making a release..." ; \
	  fi
# Create a git tag.
	@ git tag -a $(DATE) -m "Release $(DATE)."
# Upload. (This automatically makes a .tar.gz archive available on gitlab.)
	@ git push
	@ git push --tags

.PHONY: publish
publish:
# Publish an opam description.
	@ opam publish -v $(DATE) $(THIS) $(ARCHIVE) .

MENHIR_WORKING_COPY=$(HOME)/dev/menhir

.PHONY: menhir
menhir:
# Check if this is the menhir branch.
	@ if [ "$$(git symbolic-ref --short HEAD)" != "menhir" ] ; then \
	  echo "Error: this is not the menhir branch." ; \
	  git branch ; \
	  exit 1 ; \
	fi
# Copy our source files to the Menhir repository.
	@ (cd .. && cp -r fix $(MENHIR_WORKING_COPY))
# Remove a number of unneeded subdirectories.
	@ (cd $(MENHIR_WORKING_COPY)/fix && rm -rf .git demos misc)
