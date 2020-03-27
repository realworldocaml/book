/*
 * Parser for front-end configuration file.
 * ----------------------------------------------------------------
 *
 * Copyright (C) 2002 Adam Granicz, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Adam Granicz
 * Email: granicz@cs.caltech.edu
 *
 */

%{%}

%token TokEof

%token TokEq
%token TokSemi
%token TokComma
%token TokLeftBrack
%token TokRightBrack

%token <string> TokString
%token <string> TokId

%start main
%type <(string * (string * string list) list) list> main
%%

main:
   section_list TokEof        { $1 }

section_list:
   section_list_rev           { List.rev $1 }

section_list_rev:
   section                    { [$1] }
 | section_list_rev section   { $2 :: $1 }

section:
   TokLeftBrack identifier TokRightBrack assignment_list
                              { $2, $4 }

assignment_list:
   assignment_list_rev        { List.rev $1 }

assignment_list_rev:
   assignment                 { [$1] }
 | assignment_list_rev assignment
                              { $2 :: $1 }

assignment:
   identifier TokEq string_list
                              { $1, $3 }

string_list:
   string_list_rev            { List.rev $1 }

string_list_rev:
   TokString                  { [$1] }
 | string_list_rev TokComma TokString
                              { $3 :: $1 }

identifier:
   TokId                      { $1 }
