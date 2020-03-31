/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2015                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/*                                                                        */
/**************************************************************************/

 %{
 %}

%token <string> WORD
%token  LPAR
%token  RPAR
%token  COMMA
%token EOF
%start main
%type <string> main
%%
main:
type_string EOF                                                                       { $1 }
word:  WORD                                                                           { $1 }
type_string:  word                                                                    { $1 }
  | type_string word                                                                  { "'a "^$2 }
  | LPAR type_string COMMA type_string RPAR  word                                     { "('a,'b) "^$6 }
  | LPAR type_string COMMA type_string COMMA type_string RPAR word                    { "('a,'b,'c) "^$8 }
  | LPAR type_string COMMA type_string COMMA type_string COMMA type_string RPAR word  { "('a,'b,'c,'d) "^$10 }
