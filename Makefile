.PHONY: build clean

build:
	jbuilder build @site/book

clean:
	rm -rf _build *.install
