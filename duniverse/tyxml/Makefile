.PHONY: default
default: build

.PHONY: build
build:
	dune build @install

.PHONY: tools
tools:
	dune build tools/autoname.exe
	@echo "You can now use: 'dune exec tools/autoname.exe -- element'"

.PHONY: test
test:
	dune runtest

.PHONY: clean
clean:
	dune clean

.PHONY: doc
doc:
	dune build @doc

# WIKIDOC stuff
# Should have wikidoc installed with
# opam pin add https://github.com/ocsigen/wikidoc.git

DOCDIR=_wikidoc

$(DOCDIR)/.git:
	mkdir -p $(DOCDIR)
	cd $(DOCDIR) && (\
		git clone -b wikidoc git@github.com:ocsigen/tyxml.git . \
	)

.PHONY: doc wikidoc
wikidoc: build $(DOCDIR)/.git
	make -C docs wikidoc; exit 0
	rm -rf _wikidoc/docs/dev/*
	cp -r docs/api/wiki _wikidoc/docs/dev/api/
	cp -r docs/manual-wiki _wikidoc/docs/dev/manual
	git -C $(DOCDIR) add --all 
	git -C $(DOCDIR) commit -a -m "wikidoc updates"
	git -C $(DOCDIR) push origin wikidoc

