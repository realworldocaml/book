.PHONY: all clean

all:
	@jbuilder build @site
	@echo Site has been generated in _build/default/static/

code:
	jbuilder build @code

dep:
	jbuilder exec -- rwo-jbuild

clean:
	jbuilder clean
