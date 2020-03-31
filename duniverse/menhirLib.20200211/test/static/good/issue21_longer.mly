(* This file was provided by Andrej Bauer; see issue #21. *)

(* This is an example where Menhir's implementation of Pager's
   algorithm produces a conflict, even though it should not: the
   canonical automaton has no conflicts. *)

(* Infix operations a la OCaml *)
%token INFIXOP0 EQUAL

(* Names *)
%token NAME

(* Expressions and computations *)
%token NUMERAL
%token LET REC IN AND

(* End of input token *)
%token EOF

(* Precedence and fixity of infix operators *)
%start <unit> commandline

%%

(* Toplevel syntax *)

commandline:
  | LET f=NAME EQUAL e=term EOF
    { () }

  | term EOF
    { () }

(* Main syntax tree *)
term:
  | e=infix_term_
    { e }

  | LET f=NAME EQUAL c1=infix_term IN c2=term
    { () }

  | LET REC fs=separated_nonempty_list(AND, recursive_clause) IN c2=term
    { () }

infix_term:
  | n=NUMERAL
    { () }

infix_term_:
  | n=NUMERAL
    { () }

  | e2=infix_term INFIXOP0 e3=infix_term
    { () }

recursive_clause:
  | f=NAME EQUAL c=term
    { () }
