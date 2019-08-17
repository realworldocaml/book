.PHONY : test
test :
	dune exec ./source.exe
	! test -f bisect0001.out

.PHONY : clean
clean :
	dune clean
	rm -f bisect*.out
