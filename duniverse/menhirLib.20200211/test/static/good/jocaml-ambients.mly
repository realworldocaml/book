/***********************************************************************/
/*                                                                     */
/*                       Ambients in JoCaml                            */
/*                                                                     */
/*               Alan Schmitt, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1999 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

%{
open Syntax;;
%}
%token <string> IDENT STRING
%token PAR IN OUT OPEN LPAREN RPAREN LBRACKET RBRACKET NEW BANG DOT LPOINT RPOINT ZERO SEMISEMI DOLLAR

/* lowest precedence */
%right PAR
%nonassoc BANG
%right DOT
%nonassoc DOLLAR
/* highest precedence */

/* the entry point */
%start main
%type <Syntax.process> main
%%

main:
  proc SEMISEMI                         { $1 }
;

proc:
  proc PAR proc                         { Par ($1, $3) }
| LPAREN proc RPAREN                    { $2 }
| capa DOT proc                         { Seq ($1, $3) }
| DOLLAR ident LBRACKET proc RBRACKET   { Amb ($2, $4, true) }
| ident LBRACKET proc RBRACKET          { Amb ($1, $3, false) }
| NEW str DOT proc                      { New ($2, $4) }
| BANG proc                             { Bang $2 }
| LPAREN str RPAREN DOT proc            { Comm_in ($2, $5) }
| LPOINT capa RPOINT                    { Comm_out $2 }
| ZERO                                  { Zero }
;

capa:
  IN ident                              { In $2 }
| OUT ident                             { Out $2 }
| OPEN ident                            { Open $2 }
| ident                                 { Name $1 }
| STRING                                { Yell $1 }
| capa DOT capa                         { Seq_capa ($1, $3) }

ident:
  IDENT                                 { Ident $1 }

str:
  IDENT                                 { $1 }
;
