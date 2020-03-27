.PHONY: all clean test

all:
	@ dune build @all
	@ $(MAKE) --no-print-directory -C coq-minicalc $@

clean:
	@ $(MAKE) --no-print-directory -C coq-minicalc $@

test:
	@ dune build --force @test
