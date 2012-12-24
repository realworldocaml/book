DOCBOOK_XSL_PATH_Darwin = $(shell brew --prefix)/Cellar/docbook/5.0/docbook/xsl/1.76.1
DOCBOOK_XSL_PATH_Linux = /usr/share/xml/docbook/stylesheet/docbook-xsl
OS := $(shell uname -s)
PWD := $(shell pwd)
DOCBOOK_XSL_PATH ?= $(DOCBOOK_XSL_PATH_$(OS))

LINGUA:=en
CSS=rwobook
MILESTONE=alpha1

# update this if a new chapter shows up in en/
SRC=	$(addprefix $(LINGUA)/,$(shell cat chapters.$(LINGUA)))
FULLSRCS= $(addprefix $(PWD)/,$(SRC))

XMLSRCS=$(SRC:%.md=build/$(LINGUA)/source/%.xml)

all: build/$(LINGUA)/html/index.html build/$(LINGUA)/html/$(CSS).css build/$(LINGUA)/html/support/.stamp\
     build/$(LINGUA)/html/figures
	@ :

site: all
	(cd scripts && ./build.sh)
	# generate the code hilight css
	pygmentize -S trac -O linenos=1 -a .highlight -f html > commenting/build_template/ocaml_commenting/www/media/css/code.css
	python commenting/bin/generate_commenting_site.py --github-milestone $(MILESTONE)
	mkdir -p commenting-build/ocaml_commenting/www/en/$(MILESTONE)
	for i in commenting-build/ocaml_commenting/www/en/html/*.html; do \
	  cat $$i | ./scripts/html_code_highlight.native > commenting-build/ocaml_commenting/www/en/$(MILESTONE)/`basename $$i`; done

pdf: build/$(LINGUA)/pdf/rwo.pdf
	@ :

epub: build/$(LINGUA)/epub/rwo.epub
	@ :

oreilly: build/$(LINGUA)/source/rwo-oreilly.xml
	@ :

build/$(LINGUA)/html/support/.stamp:
	rm -rf build/$(LINGUA)/html/support
	cp -r web/support build/$(LINGUA)/html/support
	touch $@

build/$(LINGUA)/html/$(CSS).css: stylesheets/$(CSS).css
	cp $< $@

build/$(LINGUA)/source/.stamp:
	rm -rf build/$(LINGUA)/source
	mkdir -p build/$(LINGUA)/source
	touch $@

build/$(LINGUA)/source/rwo.xml: $(FULLSRCS)
	mkdir -p build/$(LINGUA)/source
	cd scripts && ./build.sh
	pandoc -f markdown -t docbook --chapters --template rwo.docbook -s $^ > tmp.xml
	./scripts/_build/filter_book.native < tmp.xml > $@

build/$(LINGUA)/source/rwo-oreilly.xml: $(FULLSRCS)
	mkdir -p build/$(LINGUA)/source
	cd scripts && ./build.sh
	pandoc -f markdown -t docbook --chapters --template rwo-oreilly.docbook -s $^ | ./scripts/_build/filter_book.native > $@

build/$(LINGUA)/html/index.html: build/$(LINGUA)/source/rwo.xml stylesheets/system-xsl
	xsltproc --output build/$(LINGUA)/html/ \
            stylesheets/$(LINGUA)/web.xsl build/$(LINGUA)/source/rwo.xml

build/$(LINGUA)/pdf/rwo.tex: $(FULLSRCS)
	mkdir -p build/$(LINGUA)/pdf
	pandoc -f markdown -t latex --chapters -s $^ > $@

build/$(LINGUA)/epub/rwo.epub: $(FULLSRCS)
	mkdir -p build/$(LINGUA)/epub
	pandoc -S --epub-metadata=metadata.xml -o $@ $^

build/$(LINGUA)/pdf/rwo.pdf: build/$(LINGUA)/pdf/rwo.tex
	cd build/$(LINGUA)/pdf && pdflatex rwo.tex

build/$(LINGUA)/html/figures: $(LINGUA)/figures
	ln -sf ../../../$(LINGUA)/figures $@

stylesheets/system-xsl:
	ln -sf $(DOCBOOK_XSL_PATH) $@

clean:
	rm -rf build
