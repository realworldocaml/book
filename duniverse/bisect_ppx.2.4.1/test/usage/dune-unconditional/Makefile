.PHONY : test
test : clean
	dune exec ./source.exe
	test -f bisect*.coverage

.PHONY : clean
clean :
	dune clean
	rm -f bisect*.coverage
