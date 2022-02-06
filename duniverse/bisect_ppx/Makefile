.PHONY : build
build :
	dune build -p bisect_ppx

.PHONY : watch
watch :
	dune build -p bisect_ppx -w

TEST := @runtest

.PHONY : test
test : build
	dune build -p bisect_ppx $(TEST)

.PHONY : promote
promote :
	dune promote

SELF_COVERAGE := _self

.PHONY : clean
clean :
	dune clean
	make clean-usage
	make -C test/js clean
	rm -rf $(SELF_COVERAGE)

INSTALLED_ENVIRONMENT := \
    OCAMLPATH=`pwd`/_build/install/default/lib \
    PATH=`pwd`/_build/install/default/bin:$$PATH

.PHONY : usage
usage : build
	for TEST in `ls -d test/usage/*` ; \
	do \
		echo ; \
		echo ; \
		$(INSTALLED_ENVIRONMENT) make -wC $$TEST || exit 2 ; \
	done

.PHONY : clean-usage
clean-usage :
	for TEST in `ls -d test/usage/*` ; \
	do \
		make -wC $$TEST clean ; \
	done

GH_PAGES := gh-pages

.PHONY : gh-pages
gh-pages:
	cat doc/header.html > $(GH_PAGES)/index.html
	cat README.md | node doc/convert-readme.js >> $(GH_PAGES)/index.html
	cat doc/footer.html >> $(GH_PAGES)/index.html

.PHONY : self-coverage
self-coverage : self-coverage-workspace self-coverage-rename self-coverage-test

SOURCES := bisect_ppx.opam dune-project src/

.PHONY : self-coverage-workspace
self-coverage-workspace :
	rm -rf $(SELF_COVERAGE)/bisect_ppx
	rm -rf $(SELF_COVERAGE)/meta_bisect_ppx
	mkdir -p $(SELF_COVERAGE)
	touch $(SELF_COVERAGE)/dune-workspace
	mkdir -p $(SELF_COVERAGE)/meta_bisect_ppx
	mkdir -p $(SELF_COVERAGE)/bisect_ppx
	cp -r $(SOURCES) $(SELF_COVERAGE)/meta_bisect_ppx/
	cp -r $(SOURCES) $(SELF_COVERAGE)/bisect_ppx/
	mkdir -p $(SELF_COVERAGE)/bisect_ppx/test
	cp -r test $(SELF_COVERAGE)/bisect_ppx/test/
	cd $(SELF_COVERAGE)/meta_bisect_ppx && \
	  patch --no-backup-if-mismatch -p2 < ../../test/self/meta_bisect_ppx.diff
	cd $(SELF_COVERAGE)/bisect_ppx && \
	  patch --no-backup-if-mismatch -p2 < ../../test/self/bisect_ppx.diff

.PHONY : self-coverage-rename
self-coverage-rename :
	mv \
	  $(SELF_COVERAGE)/meta_bisect_ppx/bisect_ppx.opam \
	  $(SELF_COVERAGE)/meta_bisect_ppx/meta_bisect_ppx.opam
	mv \
	  $(SELF_COVERAGE)/meta_bisect_ppx/src/common/bisect_common.ml \
	  $(SELF_COVERAGE)/meta_bisect_ppx/src/common/meta_bisect_common.ml
	mv \
	  $(SELF_COVERAGE)/meta_bisect_ppx/src/common/bisect_common.mli \
	  $(SELF_COVERAGE)/meta_bisect_ppx/src/common/meta_bisect_common.mli

FILTER := 's/^\(\(---\|+++\) [^ \t]*\).*$$/\1/g'

.PHONY : self-coverage-diff
self-coverage-diff :
	find . -name .merlin | xargs rm -f
	diff -ru src _self/meta_bisect_ppx/src | \
	  sed $(FILTER) > \
	  test/self/meta_bisect_ppx.diff || \
	  true
	diff -ru src _self/bisect_ppx/src | \
	  sed $(FILTER) > \
	  test/self/bisect_ppx.diff || \
	  true

EXPECTED_FILES := \
  --expect bisect_ppx/src/ \
  --do-not-expect bisect_ppx/src/ppx/js/ \
  --do-not-expect bisect_ppx/src/runtime/js/

.PHONY : self-coverage-test
self-coverage-test :
	cd $(SELF_COVERAGE) && rm -f bisect*.meta
	cd $(SELF_COVERAGE) && dune build @install --instrument-with meta_bisect_ppx
	cd $(SELF_COVERAGE) && \
	  dune build --force --instrument-with meta_bisect_ppx $(TEST)
	rm -rf _coverage
	cd $(SELF_COVERAGE) && \
	  _build/install/default/bin/meta-bisect-ppx-report \
	    html -o ../_coverage bisect*.meta $(EXPECTED_FILES) --verbose
	cd $(SELF_COVERAGE) && \
	  _build/install/default/bin/meta-bisect-ppx-report \
	    summary bisect*.meta --verbose
	@echo See _coverage/index.html
