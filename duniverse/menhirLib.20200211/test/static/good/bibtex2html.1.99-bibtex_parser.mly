(* Original file: bibtex2html.1.99/bibtex2html-1.99/bibtex_parser.mly *)
/**************************************************************************/
/*  bibtex2html - A BibTeX to HTML translator                             */
/*  Copyright (C) 1997-2014 Jean-Christophe Filliâtre and Claude Marché   */
/*                                                                        */
/*  This software is free software; you can redistribute it and/or        */
/*  modify it under the terms of the GNU General Public                   */
/*  License version 2, as published by the Free Software Foundation.      */
/*                                                                        */
/*  This software is distributed in the hope that it will be useful,      */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  */
/*                                                                        */
/*  See the GNU General Public License version 2 for more details         */
/*  (enclosed in the file GPL).                                           */
/**************************************************************************/

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

/*i $Id: bibtex_parser.mly,v 1.15 2010-02-22 07:38:19 filliatr Exp $ i*/

/*s Parser for BibTeX files. */

%{

  open Bibtex

%}

%token <string> Tident Tstring Tcomment
%token <string * string> Tentry
%token Tabbrev Tpreamble Tlbrace Trbrace Tcomma Tequal EOF Tsharp

%start command_list
%type <Bibtex.biblio> command_list
%start command
%type <Bibtex.command> command

%%

command_list:
  commands EOF { $1 }
;

commands:
   commands command
     { add_new_entry $2 $1 }
 | /* epsilon */
     { empty_biblio }
;
command:
   Tcomment
     { Comment $1 }
 | Tpreamble sharp_string_list Trbrace
     { Preamble $2 }
 | Tabbrev Tident Tequal sharp_string_list Trbrace
     { Abbrev (String.lowercase_ascii $2,$4) }
 | entry Tcomma comma_field_list Trbrace
     { let et,key = $1 in Entry (String.lowercase_ascii et, key, $3) }
;

entry:
 | Tentry
     { let et,key = $1 in Bibtex.current_key := key; (et,key) }

comma_field_list:
   field Tcomma comma_field_list
     { $1::$3 }
 | field
     { [$1] }
 | field Tcomma
     { [$1] }
;
field:
   field_name Tequal sharp_string_list
     { ($1,$3) }
 | field_name  Tequal
     { ($1,[String ""]) }
;
field_name:
   Tident   { String.lowercase_ascii $1 }
 | Tcomment { "comment" }
;
sharp_string_list:
   atom Tsharp sharp_string_list
     { $1::$3 }
 | atom
     { [$1] }
;
atom:
   Tident
     { Id (String.lowercase_ascii $1) }
 | Tstring
     { String $1 }
;

%%
