################################################################################
# Main targets

.PHONY: app lib atlas site \
        install_lib uninstall_lib \
				clean clean-everything

all:: site

app lib atlas site: _build
	$(MAKE) -C _build $@

################################################################################
# General Project Information

include Makefile.shared

BUILD_FILES = OCamlMakefile Makefile.shared Makefile.derived app lib book

################################################################################
# Generate meta-data

Makefile.derived::
	@if test -e .git; then                             \
	  GIT_COMMIT='Some "$(shell git rev-parse HEAD)"'; \
	else                                               \
	  GIT_COMMIT='None';                               \
	fi;                                                \
	OUTPUT="GIT_COMMIT = $$GIT_COMMIT";                 \
	if ! echo "$$OUTPUT" | cmp -s - $@; then      \
	  echo "# $$OUTPUT (UPDATED)";     								 \
	  echo "$$OUTPUT" > $@;            								 \
	fi

################################################################################
# Build everything in _build sub-directory

_build:: Makefile.derived

_build::
	@echo "# Update _build"
	@mkdir -p _build 
	@mkdir -p _build/site/css
	@rsync -a Makefile.build _build/Makefile
	@cp -ru $(BUILD_FILES) _build

################################################################################
# Install and Uninstall

OPAM_PACKAGE_NAME ?= Real-World-OCaml
export OPAM_PACKAGE_NAME

OCAMLFIND_PACKAGE_NAME = $(OPAM_PACKAGE_NAME)

_build/META:
	echo version = \"$(VERSION)\" > $@
	echo "archive(byte) = \"rwo.cma\"" >> $@
	echo "requires = \"$(PACKS)\"" >> $@

install_lib: uninstall_lib _build/META
	ocamlfind install $(OCAMLFIND_PACKAGE_NAME) \
	  _build/META \
	  _build/lib/*.cm[aixt]

uninstall_lib:
	ocamlfind remove $(OCAMLFIND_PACKAGE_NAME)

$(OPAM_PACKAGE_NAME).install:
	echo "bin: [" > $@
	echo "  "\"?_build/app/rwo\" {\"rwo\"} >> $@
	echo "]" >> $@

################################################################################
# Merlin support

.merlin: Makefile.derived
	echo "B +threads" > $@
	echo "S app" >> $@
	echo "B _build/app" >> $@
	echo "S lib" >> $@
	echo "B _build/lib" >> $@
	echo "PKG $(PACKS)" >> $@
	echo "FLG $(OCAMLFLAGS)" >> $@

.DEFAULT: .merlin

################################################################################
# Clean Up

clean:
	rm -rf \
	  _build $(OPAM_PACKAGE_NAME).install .merlin \
	  app/rwo-deps.ml.exe book/css/.sass-cache app/*.cm* lib/*.cm*
	$(MAKE) -C app clean
	$(MAKE) -C lib clean

# Should rarely need to run this. Regenerating these files requires a
# lot of additional software.
clean-everything: clean
	rm -f book/css/app.css
