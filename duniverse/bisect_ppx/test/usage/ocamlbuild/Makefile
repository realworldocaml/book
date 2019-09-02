OCAMLBUILD := \
    ocamlbuild -use-ocamlfind -no-links -byte-plugin \
	    -plugin-tag 'package(bisect_ppx-ocamlbuild)'
CONDITIONAL := -cflag -ppxopt -cflag bisect_ppx,-conditional

.PHONY : test
test : clean
	$(OCAMLBUILD) source.byte --
	! test -f bisect0001.out
	@# If the PPX option is passed correctly, there will be no output.
	BISECT_COVERAGE=YES $(OCAMLBUILD) $(CONDITIONAL) source.byte --
	! test -f bisect0001.out
	BISECT_COVERAGE=YES $(OCAMLBUILD) source.byte --
	test -f bisect0001.out

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -f bisect*.out
