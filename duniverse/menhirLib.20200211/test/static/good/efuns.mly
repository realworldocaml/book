%{
(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
%}

/* tokens */

%token EOF
%token EOL
%token SEP
%token <string>IDENT
%token TILDE

/* main entry points */

%start parse_passwd
%type <string list list> parse_passwd
%start parse_filename
%type <token list> parse_filename

%%

parse_passwd:
  passwd_line EOL parse_passwd  { $1 :: $3 }
| passwd_line EOF               { [$1] }
;

passwd_line:
  IDENT SEP passwd_line  { $1 :: $3 }
| SEP passwd_line        { "" :: $2 }
| IDENT                  { [$1] }
| /* nothing */          { [""] }
;

parse_filename:
  TILDE parse_filename   { TILDE :: $2 }
| SEP parse_filename     { SEP :: $2 }
| IDENT parse_filename   { (IDENT $1) :: $2 }
| EOF                    { [] }
;

%%
