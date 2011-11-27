notes.pdf: 00-outline.md \
	   01-prologue.md \
	   02-guided-tour.md \
	   03-modules.md \
	   04-lists-options-and-patterns.md \
	   04a-example-ascii-table.md
	markdown2pdf --toc $^  -o notes-pre.pdf
	cp notes-pre.pdf notes.pdf
	rm notes-pre.pdf


all.tmp: 00-outline.adoc \
	  01-prologue.adoc \
	  02-guided-tour.adoc \
	  03-modules.adoc \
	  04-lists-options-and-patterns.adoc \
          04a-example-ascii-table.adoc
	cat $^ > all.tmp

all.html: all.tmp
	asciidoc -b xhtml11 -a pygments $^
