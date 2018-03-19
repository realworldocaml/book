.PHONY: all clean dep publish promote test test-all

all:
	@jbuilder build @site --dev
	@echo Site has been generated in _build/default/static/

test:
	jbuilder runtest --dev

test-all:
	jbuilder build @runtest-all --dev

dep:
	jbuilder exec --dev -- rwo-jbuild

promote:
	jbuilder promote

clean:
	jbuilder clean

publish:
	rm -rf .gh-pages
	git clone `git config --get remote.origin.url` .gh-pages --reference .
	git -C .gh-pages checkout --orphan gh-pages
	git -C .gh-pages reset
	git -C .gh-pages clean -dxf
	cp -r _build/default/static/* .gh-pages/
	git -C .gh-pages add .
	git -C .gh-pages commit -m "Update Pages"
	git -C .gh-pages push origin gh-pages -f
	rm -rf .gh-pages
