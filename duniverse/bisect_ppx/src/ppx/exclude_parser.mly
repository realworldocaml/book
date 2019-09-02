/* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. */



%{

type error =
  | Invalid_file_contents
  | Invalid_file_declaration
  | Invalid_exclusion
  | Invalid_regular_expression of string

let string_of_error = function
  | Invalid_file_contents -> "invalid file contents"
  | Invalid_file_declaration -> "invalid file declaration"
  | Invalid_exclusion -> "invalid exclusion"
  | Invalid_regular_expression re -> Printf.sprintf "invalid regular expression %S" re

let fail error =
  let pos = Parsing.symbol_start_pos () in
  let line = pos.Lexing.pos_lnum in
  raise (Exclude.Exception (line, string_of_error error))

let make_regexp s =
  try Str.regexp s
  with _ -> fail (Invalid_regular_expression s)

%}

%token CLOSING_BRACKET OPENING_BRACKET
%token SEMICOLON FILE NAME REGEXP EOF
%token <string> STRING

%start file
%type <Exclude.file list> file

%%

file: file_decl_list EOF         { List.rev $1 }
| error                          { fail Invalid_file_contents }

file_decl_list: /* epsilon */    { [] }
| file_decl_list file_decl       { $2 :: $1 }

file_decl: FILE file_pattern exclusion_list separator_opt
                                 { { Exclude.path = $2;
                                     Exclude.exclusions = $3; } }
| FILE error                     { fail Invalid_file_declaration }

file_pattern:
| STRING                         { Exclude.Name $1 }
| REGEXP STRING                  { Exclude.Regexp (make_regexp $2) }

exclusion_list :
|                                { None }
| OPENING_BRACKET exclusions CLOSING_BRACKET
                                 { Some (List.rev $2) }

exclusions :
|                                { [] }
| exclusions exclusion           { $2::$1 }

exclusion: NAME STRING separator_opt
                                 { Exclude.Name $2 }
| REGEXP STRING separator_opt    { Exclude.Regexp (make_regexp $2) }
| error                          { fail Invalid_exclusion }

separator_opt: /* epsilon */     { }
| SEMICOLON                      { }

%%
