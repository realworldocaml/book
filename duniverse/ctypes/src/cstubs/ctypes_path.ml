(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Paths (long identifiers) *)

type path = string list

let is_uident s =
  Str.(string_match (regexp "[A-Z][a-zA-Z0-9_]*") s 0);;

let is_ident s =
  Str.(string_match (regexp "[A-Za-z_][a-zA-Z0-9_]*") s 0);;

let rec is_valid_path = function
  | [] -> false
  | [l] -> is_ident l
  | u :: p -> is_uident u && is_valid_path p

let path_of_string s = 
  let p = Str.(split (regexp_string ".") s) in
  if is_valid_path p then p
  else invalid_arg "Ctypes_ident.path_of_string"

let format_path fmt p =
  Format.pp_print_string fmt (String.concat "." p)
