(* This example is taken from Grun & Jacobs, second edition, p. 388. *)

(* The grammmar recognizes the language epsilon^n B C^n, that is,
   B C^n. It is not LR(1), because the number of reductions of
   A -> epsilon that must be performed before shifting B should
   (ideally) be equal to the number of C's that follow. *)

%token B C EOF
%start<unit> phrase

(* Give priority to reducing A -> epsilon over shifting B. *)

(* This means, in fact, that B can never be shifted, and that the
   parser loops forever on any input that begins with B. *)

%nonassoc B
%nonassoc reduce

%%

phrase:
  s EOF {}

s:
  a s C {}
| B     {}

a:
        {} %prec reduce

