.PHONY : test
test : clean
	dune exec ./source.exe
	! test -f bisect*.coverage
	dune clean
	BISECT_ENABLE=YES dune exec ./source.exe
	ls -l _build
	test -f bisect*.coverage
	bisect-ppx-report --html _coverage/

.PHONY : clean
clean :
	dune clean
	rm -rf bisect*.coverage _coverage
