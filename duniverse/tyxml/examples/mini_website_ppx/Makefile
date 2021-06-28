site_gen := minihtml

all:
	ocamlfind ocamlc minihtml.ml -short-paths -package tyxml.ppx -linkpkg -o ${site_gen}
	./${site_gen}

clean:
	rm -f *.cmo *.cmt *.cmi ${site_gen} index.html
