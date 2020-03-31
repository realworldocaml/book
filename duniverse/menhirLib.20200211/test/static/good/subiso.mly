 /* File tparser.mly */
%{
(***********************************************************************)
(*                                                                     *)
(*                          SubIso                                     *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Roberto Di Cosmo                                                   *)
(***********************************************************************)

(*
 $Id: tparser.mly,v 1.1 2002/03/11 16:03:27 dicosmo Exp $
*)

open Types
%}
%token <string> IDENT
%token <string> TVAR
%token PLUS TIMES ARROW
%token TYPE EQ AND
%token LPAR RPAR
%token Eof

%right PRECTYDEF
%left AND         /* lowest precedence */
%nonassoc EQ
%right ARROW
%right TIMES
%nonassoc ASSIGNS

%start main             /* the entry point */
%type <Types.ptype> main
%%
main:
texpr Eof                   { $1 }
;
texpr:
| IDENT                     { TBase $1}
| LPAR texpr RPAR           { $2 }
| texpr ARROW texpr         { TArrow($1,$3) }
| texpr TIMES texpr         { TProduct($1,$3) }
| TYPE type_binding_list %prec PRECTYDEF   { Trec(List.rev $2) }
;

type_binding_list:
	  type_binding	{[$1]}
	| type_binding_list AND type_binding	{$3 :: $1}
;
type_binding:
	       IDENT EQ texpr	{($1,$3)}
;
