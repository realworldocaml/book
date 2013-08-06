.PHONY: all depend clean distclean

all:
	cd code && $(MAKE) -j1

depend:
	./INSTALL.sh

clean:
	rm -rf scripts/_build
	cd code && $(MAKE) clean

distclean: clean
	cd code && $(MAKE) distclean
