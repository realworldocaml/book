#
# TypeScript/JSON backend
#

DUNE ?= dune

.PHONY: build
build:
	rm -f bin/atdts
	$(MAKE) clean-for-dune
	$(DUNE) build @all
	mkdir -p bin
	ln -s ../../_build/install/default/bin/atdts bin/atdts

# The symlink facilitates the development of test code that depends on the
# generated code.
.PHONY: test
test:
	$(MAKE) clean-for-dune
	# Run atdts to convert ATD -> TypeScript
	$(DUNE) runtest -f; status=$$?; \
          if [ "$$status" != 0 ]; then \
            echo "Run 'dune promote' to accept diffs, if any."; \
          fi; \
	  ln -s ../../../_build/default/atdts/test/ts-tests/everything.ts \
	    test/ts-tests/everything.ts && \
	  exit "$$status"
	# Compile and run the TypeScript code
	$(MAKE) -C test/ts-tests

.PHONY: clean-for-dune
clean-for-dune:
	rm -f test/ts-tests/everything.ts

.PHONY: clean
clean:
	$(MAKE) clean-for-dune
	$(DUNE) clean
	rm -rf bin
