TARGETS = $(addsuffix .bc, cloc count_hash dfa_restart pcregrep subst)

.PHONY: all clean

all:
	@dune build $(TARGETS)

clean:
	@dune clean
