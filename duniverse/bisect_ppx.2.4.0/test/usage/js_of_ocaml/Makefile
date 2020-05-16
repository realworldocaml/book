.PHONY : test
test : clean
	dune build ./source.bc.js
	node _build/default/source.bc.js
	! test -f bisect*.coverage
	dune clean
	BISECT_ENABLE=YES dune build ./source.bc.js
	node _build/default/source.bc.js
	ls -l _build
	test -f bisect*.coverage

.PHONY : clean
clean :
	dune clean
	rm -f bisect*.coverage
