.PHONY: default clean

ifndef BRANCH
  GITBRANCH = $(shell git branch \
            | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/' )
  ifneq ($(GITBRANCH),gh-pages)
    BRANCH = master
  else
    BRANCH = $(GITBRANCH)
  endif
endif

default:
	$(MAKE) -f Makefile.$(BRANCH)

clean:
	$(MAKE) -f Makefile.$(BRANCH) clean
	rm -f *~
