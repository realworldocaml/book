/*
 * bibtex2html - A BibTeX to HTML translator
 * Copyright (C) 1997 Jean-Christophe FILLIATRE
 *
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License version 2, as published by the Free Software Foundation.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU General Public License version 2 for more details
 * (enclosed in the file GPL).
 */

/*i $Id: condition_parser.mly,v 1.6 2003/10/03 15:37:30 marche Exp $ i*/

%{

  open Condition

%}

%token <string> IDENT STRING COMP
%token <string> INT
%token COLON AND OR NOT LPAR RPAR DOLLAR_KEY DOLLAR_TYPE EXISTS EOF

%start condition_start
%type <Condition.condition> condition_start

%left OR
%left AND
%left NOT

%%

condition_start:
  condition EOF              { $1 }
;

condition:
  condition OR condition     { Or($1,$3) }
| condition AND condition    { And($1,$3) }
| NOT condition              { Not($2) }
| LPAR condition RPAR        { $2 }
| atom                       { $1 }
;

atom:
| cte COLON STRING
    { let s = Latex_accents.normalize true $3 in
    (*i
      Printf.printf "regexp = %s\n" s;
      i*)
    Match($1, Str.regexp_case_fold s) }
| cte COMP cte
    { Comp($1,$2,$3) }
| EXISTS IDENT
    { Exists(String.uppercase $2) }
;

cte:
  IDENT                      { Field(String.uppercase $1) }
| INT                        { Cte($1) }
| STRING                     { Cte($1) }
| DOLLAR_KEY                 { Key }
| DOLLAR_TYPE                { Entrytype }
;


