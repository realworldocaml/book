(* Original file: mlpost.0.8.2/mlpost-0.8.2/dvi/pfb_parser.mly *)
/**************************************************************************/
/*                                                                        */
/*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       */
/*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           */
/*                                                                        */
/*  This software is free software; you can redistribute it and/or        */
/*  modify it under the terms of the GNU Library General Public           */
/*  License version 2.1, with the special exception on linking            */
/*  described in file LICENSE.                                            */
/*                                                                        */
/*  This software is distributed in the hope that it will be useful,      */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  */
/*                                                                        */
/**************************************************************************/

/* File parser.mly */
%{
  open Fonts_type

  let encoding_table = ref (Array.create 256 "")
%}
%token <string> NAME_CHARSTRING, NAME_ENCODING
%token <int> ID_ENCODING
%token DUMB
%type <(string array) * (string list)> pfb_human_main
%type <string list> enc_main
%start pfb_human_main enc_main
%%
pfb_human_main :
DUMB encoding DUMB charstrings DUMB{ 
  let rencoding_table = !encoding_table in
  encoding_table := Array.create 256 "";
  (rencoding_table,$4)}

encoding :
| {}
| ID_ENCODING NAME_ENCODING encoding {(!encoding_table).($1)<-$2 }

charstrings :
| {[]}
| NAME_CHARSTRING charstrings { $1::$2}

enc_main :
| DUMB enc_aux DUMB {$2}

enc_aux :
| {[]}
| NAME_ENCODING enc_aux {$1::$2}
