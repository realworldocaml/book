PKG=unix,compiler-libs.toplevel,ppx_sexp_conv,findlib.top

all:
	ocamlfind c -thread -o ocaml-topexpect -linkpkg -linkall -package $(PKG) main.ml
	$(MAKE) -C ppx-ignore all

clean:
	rm -f *.cm*
	$(MAKE) -C ppx-ignore clean

distclean: clean
	rm -f ocaml-topexpect
