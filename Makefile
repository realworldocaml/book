.PHONY: all clean

all:
	jbuilder build @site

code:
	jbuilder build @code

dep:
	jbuilder exec -- rwo-jbuild

clean:
	jbuilder clean
