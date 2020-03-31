(* Original file: dolmen.0.2/dolmen-0.2/src/languages/dimacs/parseDimacs.mly *)

(* This file is free software, part of dolmen. See file "LICENSE" for more details *)

/* Functor parameters */

%parameter <L : ParseLocation.S>
%parameter <T : Ast_dimacs.Term with type location := L.t>
%parameter <S : Ast_dimacs.Statement with type location := L.t and type term := T.t>

/* Starting symbols */

%start <S.t list> file
%start <S.t option> input

%%

input:
  | NEWLINE i=input
    { i }
  | p=start
    { Some p }
  | c=clause
    { Some c }
  | EOF
    { None }

file:
  | NEWLINE* h=start l=cnf
    { h :: l }

start:
  | P CNF nbvar=INT nbclause=INT NEWLINE
    { let loc = L.mk_pos $startpos $endpos in
      S.p_cnf ~loc nbvar nbclause }

cnf:
  | EOF
    { [] }
  | NEWLINE l=cnf
    { l }
  | c=clause l=cnf
    { c :: l }

clause:
  | c=nonempty_list(atom) ZERO NEWLINE
    { let loc = L.mk_pos $startpos $endpos in S.clause ~loc c }

atom:
  | i=INT
    { let loc = L.mk_pos $startpos $endpos in T.atom ~loc i }
