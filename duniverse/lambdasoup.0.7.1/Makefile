LIB := lambdasoup
VERSION := 0.6.2

.PHONY : build
build :
	dune build

.PHONY : test
test :
	dune runtest --no-buffer -j 1

BISECT_FILES_PATTERN := _build/default/test/bisect*.out
COVERAGE_DIR := _coverage

.PHONY : coverage
coverage :
	BISECT_ENABLE=yes dune build @build-test
	rm -rf $(BISECT_FILES_PATTERN)
	(cd _build/default/test && ./test.exe)
	bisect-ppx-report html --expect src/ --do-not-expect src/lambdasoup.ml
	bisect-ppx-report summary
	@echo See $(COVERAGE_DIR)/index.html

BS4_MISSING := Beautiful Soup not installed. Skipping Python performance test.

.PHONY : performance-test
performance-test :
	dune build @build-performance-test
	(cd _build/default/test/performance && ./performance.exe)
	@((python -c "import bs4" 2> /dev/null \
	  || (echo $(BS4_MISSING); exit 1)) \
	  && (echo python test/performance/performance.py; \
	           python test/performance/performance.py)) \
	  || exit 0

# The docs targets are inactive for the time being.
HTML := docs/html
DOCFLAGS := -docflags -colorize-code

.PHONY : docs
docs : docs-postprocess
	$(OCAMLBUILD) $(DOCFLAGS) docs/soup.docdir/index.html
	rm -rf $(HTML)
	mkdir -p $(HTML)
	rsync -r _build/docs/soup.docdir/* $(HTML)/
	cp docs/style.css $(HTML)/
	rm $(HTML)/index*.html
	_build/docs/postprocess.native < $(HTML)/Soup.html > $(HTML)/index.html
	rm $(HTML)/Soup*.html $(HTML)/type_Soup*.html $(HTML)/*.stamp
	@echo "\nSee docs/html/index.html"

.PHONY : docs-postprocess
docs-postprocess :
	$(OCAMLBUILD) postprocess.native

GHPAGES_REPO := scratch/docs-publish

.PHONY : publish-docs
publish-docs : check-doc-prereqs docs
	@[ -d $(GHPAGES_REPO)/.git ] \
		|| (echo "\nPlease create a repository in $(GHPAGES_REPO)"; exit 1)
	cp $(HTML)/* $(GHPAGES_REPO)
	cd $(GHPAGES_REPO) \
		&& git add -A \
		&& git commit --amend --reset-author -m "Lambda Soup documentation." \
		&& git push -f

DOC_ZIP := docs/$(LIB)-$(VERSION)-doc.zip

.PHONY : package-docs
package-docs : docs
	rm -f $(DOC_ZIP)
	zip -9j $(DOC_ZIP) $(HTML)/*

.PHONY : clean
clean :
	dune clean
	rm -rf $(COVERAGE_DIR)
	# rm -rf docs/html
