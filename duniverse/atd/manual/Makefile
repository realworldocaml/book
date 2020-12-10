# `make pdf' requires pdflatex and builds readme.pdf
# `make txt' requires hevea and builds readme.txt
# `make html' requires hevea and builds readme.html

TEXFILES = atd-manual atd-body

.PHONY: all pdf txt html clean

all: pdf txt html
pdf: atd-manual.pdf
txt: atd-manual.txt
html: atd-manual.html

atd-manual.tex: ../atd_version.ml atd-manual.mlx
	OCAMLPATH=../..:$$OCAMLPATH camlmix atd-manual.mlx -o atd-manual.tex

atd-body.tex: ../atd_version.ml macros.ml atd-body.mlx
	OCAMLPATH=../..:$$OCAMLPATH camlmix atd-body.mlx -o atd-body.tex

atd-manual.txt: $(addsuffix .tex, $(TEXFILES))
	rm -f *.aux
	hevea -fix -text atd-manual
	iconv -f ISO_8859-1 -t UTF-8 < atd-manual.txt > ../atd-manual.txt

atd-manual.html: $(addsuffix .tex, $(TEXFILES))
	rm -f *.aux
	hevea -fix atd-manual

atd-manual.pdf: $(addsuffix .tex, $(TEXFILES))
	pdflatex atd-manual
	pdflatex atd-manual

clean:
	rm -f *.aux *.toc *.log *.out *.haux *.htoc *.fls *.mlx.ml \
		atd-manual.pdf atd-manual.txt atd-manual.html
