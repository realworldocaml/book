(* This grammar must be rejected, as the expansion of parameterized
   nonterminal symbols does not terminate: seq(t) calls seq(wrap(t)).

   This problem has been reported by Stéphane Lescuyer
   and studied by Yann Régis-Gianas.

   See https://gitlab.inria.fr/fpottier/menhir/issues/4

 *)

%token A

%%

start:
 seq(A) {}

wrap(t):
 t {}

seq(t):
| wrap(seq(wrap(t))) {}
