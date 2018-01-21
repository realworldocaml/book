.PHONY: all clean

all:
	jbuilder build @site

code:
	jbuilder build @code

dep:
	jbuilder build @jbuilder

clean:
	jbuilder clean
