DOCBOOK_XSL_PATH_Darwin = $(shell brew --prefix)/Cellar/docbook/5.0/docbook/xsl/1.76.1
DOCBOOK_XSL_PATH_Linux = /usr/share/xml/docbook/stylesheet/docbook-xsl
OS := $(shell uname -s)
PWD := $(shell pwd)
DOCBOOK_XSL_PATH ?= $(DOCBOOK_XSL_PATH_$(OS))

LINGUA:=en
MILESTONE=alpha1

# update this if a new chapter shows up in en/
SRC=	$(addprefix $(LINGUA)/,$(shell cat chapters.$(LINGUA)))
FULLSRCS= $(addprefix $(PWD)/,$(SRC))

XMLSRCS=$(SRC:%.md=build/$(LINGUA)/source/%.xml)

all: build/$(LINGUA)/html/index.html
	@ :

milestone-%: all
	./gen-milestone.sh $*

server:
	cd commenting-ocaml && ocamlbuild -use-ocamlfind rwo.native

oreilly: build/$(LINGUA)/source/rwo-oreilly.xml
	@ :

build/$(LINGUA)/source/.stamp:
	rm -rf build/$(LINGUA)/source
	mkdir -p build/$(LINGUA)/source
	touch $@

build/$(LINGUA)/source/rwo.xml: $(FULLSRCS)
	mkdir -p build/$(LINGUA)/source
	cd scripts && ./build.sh
	pandoc -f markdown -t docbook --chapters --template rwo.docbook -s $^ | ./scripts/_build/filter_book.native > $@

build/$(LINGUA)/source/rwo-oreilly.xml: $(FULLSRCS)
	mkdir -p build/$(LINGUA)/source
	cd scripts && ./build.sh
	pandoc -f markdown -t docbook --chapters --template rwo-oreilly.docbook -s $^ | ./scripts/_build/filter_book.native > $@

build/$(LINGUA)/html/index.html: build/$(LINGUA)/source/rwo.xml stylesheets/system-xsl
	xsltproc --output build/$(LINGUA)/html/ \
            stylesheets/$(LINGUA)/web.xsl build/$(LINGUA)/source/rwo.xml

stylesheets/system-xsl:
	ln -sf $(DOCBOOK_XSL_PATH) $@

clean:
	rm -rf build
