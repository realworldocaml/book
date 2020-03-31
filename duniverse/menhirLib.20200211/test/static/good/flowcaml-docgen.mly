/**************************************************************************/
/*                                                                        */
/*                               Flow Caml                                */
/*                                                                        */
/*          Vincent Simonet, Projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*  Copyright 2002, 2003 Institut National de Recherche en Informatique   */
/*  et en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.                  */
/*                                                                        */
/*  Author contact: Vincent.Simonet@inria.fr                              */
/*  Software page: http://cristal.inria.fr/~simonet/soft/flowcaml/        */
/*                                                                        */
/**************************************************************************/

/* $Id: doc_parser.mly,v 1.4 2003/06/26 13:32:48 simonet Exp $ */

%{
open Doc_parsetree

%}


/***************************************************************************/

%token <int> CLOSE
%token <string> DIRECTIVE
%token EOF
%token <string> LBRACE
%token <int> OPEN
%token RBRACE
%token <int*int> TERMINATE
%token <string> TEXT
%token <string> VERB
%start file
%type <Doc_parsetree.comment list> file

%%

file:
  EOF
    { [] }
| OPEN ftext CLOSE file
    {
      { cmt_start = $1;
	cmt_end = $3;
	cmt_content = Ftext (List.rev $2)
      } :: $4 }
| TERMINATE file
    {
      { cmt_start = fst $1;
	cmt_end = snd $1 ;
	cmt_content = Terminate
      } :: $2 }
;

ftext:
  /*empty*/
    { [] }
| ftext DIRECTIVE
    { (Directive $2) :: $1 }
| ftext TEXT
    { (String $2) :: $1 }
| ftext LBRACE ftext RBRACE
    { (Block ($2, List.rev $3)) :: $1 }
| ftext VERB
    { (Block ("src", [String $2])) :: $1 }
;
