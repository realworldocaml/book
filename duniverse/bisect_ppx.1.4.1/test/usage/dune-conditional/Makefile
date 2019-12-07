.PHONY : test
test : clean
	dune exec ./source.exe
	! test -f bisect0001.out
	dune clean
	BISECT_ENABLE=YES dune exec ./source.exe
	ls -l _build
	test -f bisect0001.out

.PHONY : clean
clean :
	dune clean
	rm -f bisect*.out
