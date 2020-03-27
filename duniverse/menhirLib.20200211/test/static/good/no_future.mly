/* This grammar describes a nonempty list of A's, followed with EOF.
   It is LR(2), not LR(1). There is a shift/reduce conflict, which we
   resolve in favor of shifting. As a result, the production listA ->
   can never be reduced (Menhir warns about this), and the resulting
   automaton rejects every sentence of the form A+ EOF: it reads all
   of the A's, then, upon seeing EOF, rejects. */

/* This automaton does *not* have the property that a syntax error is
   detected as soon as possible, or (stated differently) that, as
   long as the automaton continues reading, a viable future exists.
   Indeed, this automaton accepts the *empty* language, so, for the
   property to be satisfied, it should always fail immediately, without
   reading anything. Yet, it will read an arbitrarily long sequence of
   A's and fail only upon encountering EOF. */

/* The problem has nothing to do with merging of states. Indeed, even
   the canonical LR(1) automaton exhibits this problem. */

%token A EOF
%start<unit> main

%nonassoc empty_list
%nonassoc A

%%

listA:
          %prec empty_list {}
| A listA {}

main:
  listA A EOF {}
