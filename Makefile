DOCBOOK_XSL_PATH_Darwin = /usr/local/Cellar/docbook/5.0/docbook/xsl/1.76.1
DOCBOOK_XSL_PATH_Linux = /usr/share/xml/docbook/stylesheet/docbook-xsl
OS := $(shell uname -s)
DOCBOOK_XSL_PATH ?= $(DOCBOOK_XSL_PATH_$(OS))

LINGUA:=en
CSS=rwobook

# update this if a new chapter shows up in en/
SRC=	$(notdir $(wildcard $(LINGUA)/[0-9]*.md))

XMLSRCS=$(SRC:%.md=build/$(LINGUA)/source/%.xml)

all: build/$(LINGUA)/html/index.html build/$(LINGUA)/html/$(CSS).css build/$(LINGUA)/html/support/.stamp
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

build/$(LINGUA)/source/00book.xml: $(LINGUA)/00book.xml build/$(LINGUA)/source/.stamp
	mkdir -p build/$(LINGUA)/source
	cp $< build/$(LINGUA)/source/00book.xml

build/$(LINGUA)/source/%.xml: $(LINGUA)/%.md
	pandoc -f markdown -t docbook --chapters $< > $@

build/$(LINGUA)/html/index.html: build/$(LINGUA)/source/00book.xml $(XMLSRCS) stylesheets/system-xsl
	xsltproc --output build/$(LINGUA)/html/ \
            stylesheets/$(LINGUA)/web.xsl build/$(LINGUA)/source/00book.xml

stylesheets/system-xsl:
	ln -sf $(DOCBOOK_XSL_PATH) $@

clean:
	rm -rf build
