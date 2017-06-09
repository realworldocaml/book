.PHONY: build clean

build:
	jbuilder build @site/book

build-nondeterministic:
	RUN_NONDETERMINISTIC=Y jbuilder build @site/book

clean:
	rm -rf _build *.install
