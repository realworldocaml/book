.PHONY : test
test : clean
	dune exec ./source.exe
	! test -f bisect*.coverage
	dune clean
	BISECT_ENABLE=YES dune exec ./source.exe
	ls -l _build
	test -f bisect*.coverage

.PHONY : clean
clean :
	dune clean
	rm -f bisect*.coverage
