.PHONY : build
build :
	dune build

.PHONY : test
test : build
	dune build @tester
	cd _build/default/test/unit && ./test_main.exe -runner sequential

.PHONY : clean
clean :
	dune clean
	for TEST in `ls -d test/usage/*` ; \
	do \
		make -wC $$TEST clean ; \
	done

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

PRESERVE := _build/default/test/unit/_preserve

.PHONY : save-test-output
save-test-output :
	(cd $(PRESERVE) && find ./fixtures -name '*reference.*') \
	  | xargs -I FILE cp $(PRESERVE)/FILE test/unit/FILE

GH_PAGES := gh-pages

.PHONY : gh-pages
gh-pages:
	opam list --installed lambdasoup
	opam list --installed omd
	dune build doc/postprocess.exe
	rm -rf $(GH_PAGES)
	git clone git@github.com:aantron/bisect_ppx.git $(GH_PAGES)
	cd $(GH_PAGES) && \
	  git checkout gh-pages
	omd README.md | dune exec doc/postprocess.exe > $(GH_PAGES)/index.html
	cd $(GH_PAGES) && \
	  git add -A && \
	  git commit --amend --no-edit && \
	  git push -f
