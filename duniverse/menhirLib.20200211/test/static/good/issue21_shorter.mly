(* This file was provided by Andrej Bauer; see issue #21. *)

(* This is an example where Menhir's implementation of Pager's
   algorithm produces a conflict, even though it should not: the
   canonical automaton has no conflicts. *)

(* Infix operations a la OCaml *)
%token PLUS

(* Names *)
%token NAME

(* Expressions and computations *)
%token LET REC IN

(* End of input token *)
%token EOF

(* Precedence and fixity of infix operators *)
%start <unit> commandline

%%

(* Toplevel syntax *)

commandline:
  | LET term EOF
    { () }

  | term EOF
    { () }

(* Main syntax tree *)
term:
  | NAME
    { () }

  | name PLUS
    { () }

  | LET name IN term
    { () }

  | REC term IN term
    { () }

name:
  | NAME
    { () }
