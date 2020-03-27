/*
    FNF: Free Netlist Format
    Copyright (C) 2004 Tom Hawkins (tomahawkins@yahoo.com)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
*/

%{
(* Pre Code *)
open Fnf_core;;

let parse_error s =
  Parser_util.error ("Unexpected token: '" ^ (Parser_util.get_current_token ()) ^ "'")
;;

let debug msg =
  print_string msg; print_newline ()
;;

let format_string str =
  let str = String.sub str 1 (String.length str - 2) in
  let len = String.length str in
  let rec format_string sofar i =
    if i >= len then sofar else
      match str.[i] with
      | '\\' -> format_string (sofar ^ String.make 1 '\\' ^ String.make 1 str.[i + 1]) (i + 2)
      | c    -> format_string (sofar ^ String.make 1 c) (i + 1)
  in
  format_string "" 0
;;

let scope_stack = ref [];;

let current_scope () =
  List.hd !scope_stack
;;

let push_scope scope =
  scope_stack := scope :: !scope_stack
;;

let pop_scope () =
  scope_stack := List.tl !scope_stack
;;

let is_root_scope () =
  !scope_stack = []
;;

let id_cell_table = Hashtbl.create 1024;;

let port_id_table = Hashtbl.create 1024;;

let clear_tables () =
  scope_stack := [];
  Hashtbl.clear id_cell_table;
  Hashtbl.clear port_id_table
;;

let add_cell id cell =
  if Hashtbl.mem id_cell_table id then
    Parser_util.error ("Duplicate cell ID: " ^ string_of_int id)
  else
    Hashtbl.add id_cell_table id cell
;;

let add_port port id =
  Hashtbl.add port_id_table port id
;;

let connect_ports () =
  Hashtbl.iter (fun port id ->
    try
      connect (Hashtbl.find id_cell_table id) port
    with Not_found ->
      Parser_util.error ("Reference cell ID not found: " ^ string_of_int id)
  ) port_id_table;
;;


%}

// FNF Keywords and Symbols

%token <string * (string * int * int)> Parenl
%token <string * (string * int * int)> Parenr
%token <string * (string * int * int)> Scope
%token <string * (string * int * int)> Input
%token <string * (string * int * int)> Output
%token <string * (string * int * int)> Name
%token <string * (string * int * int)> Dangle
%token <string * (string * int * int)> Const
%token <string * (string * int * int)> Buf
%token <string * (string * int * int)> Not
%token <string * (string * int * int)> And
%token <string * (string * int * int)> Xor
%token <string * (string * int * int)> Or
%token <string * (string * int * int)> Concat
%token <string * (string * int * int)> Select
%token <string * (string * int * int)> Eq
%token <string * (string * int * int)> Lt
%token <string * (string * int * int)> Add
%token <string * (string * int * int)> Sub
%token <string * (string * int * int)> Mul
%token <string * (string * int * int)> Mux
%token <string * (string * int * int)> Ff
%token <string * (string * int * int)> Ffc
%token <string * (string * int * int)> Bbox
%token <string * (string * int * int)> Integer
%token <string * (string * int * int)> String
%token <string * (string * int * int)> EOF
%token <string * (string * int * int)> Lexer_error

%start netlist
%type <Fnf_core.scope> netlist

%%

netlist
  : scope  { connect_ports (); let scope = current_scope () in clear_tables (); scope }
  ;

scope
  : Parenl Scope scope_name Parenl scope_items Parenr Parenr  { () }
  ;

scope_name
  : name name { push_scope (if is_root_scope () then create_root_scope $1 else create_sub_scope (current_scope ()) $1 $2) }
  ;

scope_items
  :                         { () }
  | scope_items scope_item  { () }
  ;

scope_item
  : scope  { pop_scope () }
  | cell   { () }
  ;

string
  : String { format_string (fst $1) }
  ;

integer
  : Integer { int_of_string (fst $1) }
  ;

integers_0
  :                    { [] }
  | integers_0 integer { $2 :: $1 }
  ;

integers
  : integers_0 { List.rev $1 }

width
  : integer { $1 }
  ;

name
  : string { $1 }
  ;

id
  : integer { $1 }
  ;

bit
  : integer { $1 }
  ;

cell
  : Parenl Input  id name width                  Parenr  { let cell = create_input (current_scope ()) $4 $5 in add_cell $3 cell }
  | Parenl Output id name width id               Parenr  { let cell, data = create_output (current_scope ()) $4 $5 in add_cell $3 cell; add_port data $6 }
  | Parenl Name   id name width id               Parenr  { let cell, data = create_name   (current_scope ()) $4 $5 in add_cell $3 cell; add_port data $6 }
  | Parenl Dangle id                             Parenr  { add_cell $3 (dangle (current_scope ())) }
  | Parenl Const  id string                      Parenr  { let cell = create_const (current_scope ()) $4 in add_cell $3 cell }
  | Parenl Buf    id width id                    Parenr  { let cell, data = create_buf (current_scope ()) $4 in add_cell $3 cell; add_port data $5 }
  | Parenl Not    id width id                    Parenr  { let cell, data = create_not (current_scope ()) $4 in add_cell $3 cell; add_port data $5 }
  | Parenl And    id width id id                 Parenr  { let cell, left, right = create_and  (current_scope ()) $4 in add_cell $3 cell; add_port left $5; add_port right $6 }
  | Parenl Xor    id width id id                 Parenr  { let cell, left, right = create_xor  (current_scope ()) $4 in add_cell $3 cell; add_port left $5; add_port right $6 }
  | Parenl Or     id width id id                 Parenr  { let cell, left, right = create_or   (current_scope ()) $4 in add_cell $3 cell; add_port left $5; add_port right $6 }
  | Parenl Concat id width width id id           Parenr  { let cell, left, right = create_concat (current_scope ()) $4 $5 in add_cell $3 cell; add_port left $6; add_port right $7 }
  | Parenl Select id width bit id                Parenr  { let cell, data = create_select (current_scope ()) $4 $5 in add_cell $3 cell; add_port data $6 }
  | Parenl Eq     id width id id                 Parenr  { let cell, left, right = create_eq   (current_scope ()) $4 in add_cell $3 cell; add_port left $5; add_port right $6 }
  | Parenl Lt     id width id id                 Parenr  { let cell, left, right = create_lt   (current_scope ()) $4 in add_cell $3 cell; add_port left $5; add_port right $6 }
  | Parenl Add    id width id id                 Parenr  { let cell, left, right = create_add  (current_scope ()) $4 in add_cell $3 cell; add_port left $5; add_port right $6 }
  | Parenl Sub    id width id id                 Parenr  { let cell, left, right = create_sub  (current_scope ()) $4 in add_cell $3 cell; add_port left $5; add_port right $6 }
  | Parenl Mul    id width id id                 Parenr  { let cell, left, right = create_mul  (current_scope ()) $4 in add_cell $3 cell; add_port left $5; add_port right $6 }
  | Parenl Mux    id width id id id              Parenr  { let cell, select, data_0, data_1 = create_mux (current_scope ()) $4 in add_cell $3 cell; add_port select $5; add_port data_0 $6; add_port data_1 $7 }
  | Parenl Ff     id width id id                 Parenr  { let cell, clock, data = create_ff   (current_scope ()) $4 in add_cell $3 cell; add_port clock $5; add_port data $6 }
  | Parenl Ffc    id width id id id              Parenr  { let cell, clear, clock, data = create_ffc (current_scope ()) $4 in add_cell $3 cell; add_port clear $5; add_port clock $6; add_port data $7 }
  | Parenl Bbox   id string width width Parenl integers Parenr id Parenr
    { let cell, data = create_bbox (current_scope ()) $4 $5 $6 $8 in add_cell $3 cell; add_port data $10 }
  ;

%%
(* Post Code *)

