BISECT := -package bisect_ppx -ppxopt bisect_ppx,--conditional

.PHONY : test
test : clean
	ocamlfind opt -linkpkg source.ml
	./a.out
	! test -f bisect*.coverage
	@# If the PPX option is passed correctly, there will be no output.
	ocamlfind opt -linkpkg $(BISECT) source.ml
	./a.out
	! test -f bisect*.coverage
	BISECT_ENABLE=YES ocamlfind opt -linkpkg $(BISECT) source.ml
	./a.out
	test -f bisect*.coverage

.PHONY : clean
clean :
	rm -f *.cm* *.o a.out bisect*.coverage
