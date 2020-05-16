all:
	$(MAKE) -C src all
	$(MAKE) -C toplevel all

test:
	$(MAKE) -C test all

clean:
	$(MAKE) -C src clean
	$(MAKE) -C toplevel clean
	$(MAKE) -C test clean

install:
	$(MAKE) -C src install
	$(MAKE) -C toplevel install

findlib-install:
	$(MAKE) -C src findlib-install
	$(MAKE) -C toplevel install

uninstall:
	$(MAKE) -C src uninstall
	$(MAKE) -C toplevel uninstall

findlib-uninstall:
	$(MAKE) -C src findlib-uninstall
	$(MAKE) -C toplevel uninstall

.PHONY: all test clean install uninstall findlib-install findlib-uninstall
