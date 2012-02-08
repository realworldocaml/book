DOCBOOK_XSL_PATH ?= /usr/local/Cellar/docbook/5.0/docbook/xsl/1.76.1

LINGUA:=en
CSS=rwobook

all: build/$(LINGUA)/html/index.html build/$(LINGUA)/html/$(CSS).css build/$(LINGUA)/html/support/.stamp
	@ :

build/$(LINGUA)/html/support/.stamp:
	rm -rf build/$(LINGUA)/html/support
	cp -r web/support build/$(LINGUA)/html/support
	touch $@

build/$(LINGUA)/html/$(CSS).css: stylesheets/$(CSS).css
	cp $< $@
	
build/$(LINGUA)/source/00book.xml: $(LINGUA)/00book.xml
	mkdir -p build/$(LINGUA)/source
	cp $(LINGUA)/*.xml build/$(LINGUA)/source/

build/$(LINGUA)/html/index.html: build/$(LINGUA)/source/00book.xml stylesheets/system-xsl
	xsltproc --output build/$(LINGUA)/html/ \
            stylesheets/$(LINGUA)/web.xsl build/$(LINGUA)/source/00book.xml

stylesheets/system-xsl:
	ln -sf $(DOCBOOK_XSL_PATH) $@

clean:
	rm -rf build
