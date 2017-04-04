PKG=unix,compiler-libs.toplevel,toplevel_expect_test.types

all:
	ocamlfind c -o ocaml-topexpect -linkpkg -package $(PKG) main.ml

clean:
	rm -f *.cm*

distclean: clean
	rm -f ocaml-topexpect
