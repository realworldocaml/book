.PHONY: all clean

all: site

APP?=rwo-build
APP_DEPS?=rwo-deps

HTML_CHAPTER_FILES := \
  00-prologue.html \
  01-guided-tour.html \
  02-variables-and-functions.html \
  03-lists-and-patterns.html \
  04-files-modules-and-programs.html \
  05-records.html \
  06-variants.html \
  07-error-handling.html \
  08-imperative-programming.html \
  09-functors.html \
  10-first-class-modules.html \
  11-objects.html \
  12-classes.html \
  13-maps-and-hashtables.html \
  14-command-line-parsing.html \
  15-json.html \
  16-parsing-with-ocamllex-and-menhir.html \
  17-data-serialization.html \
  18-concurrent-programming.html \
  19-foreign-function-interface.html \
  20-runtime-memory-layout.html \
  21-garbage-collector.html \
  22-compiler-frontend.html \
  23-compiler-backend.html

HTML_FILES := $(HTML_CHAPTER_FILES) index.html faqs.html toc.html install.html

################################################################################
# Compute chapters dependencies

.depend: $(foreach file,$(HTML_CHAPTER_FILES),book/$(file))
	$(APP_DEPS) deps site -repo-root ./ > $@
	if [ ! -e examples ]; then ln -sf ../examples; fi
	mkdir -p site

################################################################################
# SASS to CSS style sheet preprocessing

ifndef SASS
SASS:=$(shell command -v sassc 2> /dev/null)
endif
ifndef SASS
SASS:=$(shell command -v sassc 2> /dev/null)
endif

book/css/app.css: $(wildcard	book/scss/*.scss)
ifndef SASS
	$(error "Couldn't find sass or sassc binary, install one or adjust SASS variable")
endif
	$(SASS) -q -I $(shell opam config var foundation:lib)/scss book/scss/app.scss > $@

################################################################################
# HTML Book

ROOT = ./

site/%.html: book/%.html
ifdef PYGMENTIZE
	$(APP) build  chapter -pygmentize -code examples -o site/ -chapter $$(echo $< | sed -r "s/.*([0-9]{2}).*/\1/") -repo-root $(ROOT) $<
else
	$(APP) build  chapter -o site/ -code examples  -chapter $$(echo $< | sed -r "s/.*([0-9]{2}).*/\1/") -repo-root $(ROOT) $<
endif

site/index.html:
	$(APP) build frontpage -o site/ -repo-root $(ROOT)

site/faqs.html: book/faqs.html
	$(APP) build faqs -o site/ -repo-root $(ROOT)

site/install.html: book/install.html
	$(APP) build install -o site/ -repo-root $(ROOT)

site/toc.html:
	$(APP) build toc -o site/ -repo-root $(ROOT)

site-aux::
	rsync -av static/images static/js static/css site/

site: $(foreach file,$(HTML_FILES),site/$(file)) site-aux

################################################################################
# Book in format required by O'Reilly's Atlas

atlas/%.html: book/%.html
	$(APP) build chapter -o atlas/ -repo-root ./ $<

atlas: $(foreach file,$(HTML_CHAPTER_FILES),atlas/$(file))

clean:
	rm -f .depend
	rm -rf site

-include .depend
