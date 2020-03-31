(* Submitted by Thierry Belloeil -- thanks. *)

(* This grammar shows a problem with the conflict explanation module.
   The %nonassoc directives force a shift/reduce conflict to be resolved
   in favor of reduction. This cuts a transition in the automaton, which
   in turn prevents another shift/reduce conflict from being explained. *)

(* Beware: removing the %nonassoc or %prec directives causes the final
   numbering of states to change (but the raw numbers remain the same). *)

%token SIZE LOG EOF ARROW BANG

%nonassoc BANG
%nonassoc below_NUM

%start desc

%type <unit> desc

%%

desc:
  exprl ARROW expr EOF {}

expr:
  | expr BANG  {}
  | expr SIZE  {}
  | LOG exprl  {}

exprl:
    BANG exprl {}
  | SIZE exprl {}
  |	       {} %prec below_NUM

