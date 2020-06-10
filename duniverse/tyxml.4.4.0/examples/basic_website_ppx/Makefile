site_gen := make_site

all:
	ocamlfind ocamlc site_html.ml -package tyxml.ppx -short-paths -linkpkg -o ${site_gen}
	./${site_gen}

clean:
	rm -f *.cmo *.cmt *.cmi ${site_gen} index.html
