MENHIR            := menhir
MENHIRFLAGS       := --coq
COQINCLUDES       :=

Makefile.coq:
	coq_makefile -o Makefile.coq Lexer.v MiniCalc.v Parser.v $(COQINCLUDES)

Parser.v : Parser.vy
	$(MENHIR) $(MENHIRFLAGS) $<

ifneq ($(MAKECMDGOALS),clean)
-include Makefile.coq
endif

.PHONY: clean
clean::
	@ rm -f Parser.v
	@ rm -f Makefile.coq Makefile.coq.conf
	@ rm -f .*.aux
