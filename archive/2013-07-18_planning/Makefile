MDS=	00-outline.md \
	01-prologue.md \
	02-guided-tour.md \
	03-modules.md \
	04-lists-options-and-patterns.md \
	04a-example-ascii-table.md

# XSL as installed on MacOS X homebrew's "docbook" keg
FO_XSL?=/usr/local/Cellar/docbook/5.0/docbook/xsl/1.76.1/fo/docbook.xsl

notes.pdf: $(MDS)
	markdown2pdf --toc $^  -o notes-pre.pdf
	cp notes-pre.pdf notes.pdf
	rm notes-pre.pdf

notes.xml: $(MDS)
	pandoc -s --from=markdown --to=docbook $^ > $@

%.fo: %.xml
	xsltproc --output $@ --stringparam fop1.extensions 1 $(FO_XSL) $<

%.docbook.pdf: %.fo
	fop -fo $< -pdf $@

all.tmp: 00-outline.adoc \
	  01-prologue.adoc \
	  02-guided-tour.adoc \
	  03-modules.adoc \
	  04-lists-options-and-patterns.adoc \
          04a-example-ascii-table.adoc
	cat $^ > all.tmp

all-sh.html: all.tmp
	asciidoc -b xhtml11 $^

all.html: all.tmp
	a2x -f xhtml $^

# This is a temporary hack, until we have a think we're finally agreed
# on as to how this will all works
temp.pdf: 03-modules.md
	markdown2pdf --toc $^ -o z.pdf
	cp z.pdf temp.pdf
	rm z.pdf

