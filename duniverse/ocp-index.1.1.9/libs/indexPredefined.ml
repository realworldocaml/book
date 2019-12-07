(**************************************************************************)
(*                                                                        *)
(*  Copyright 2013 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the Lesser GNU Public License version 3.0.                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

open IndexTypes
open Outcometree

(* List and doc taken from
   http://caml.inria.fr/pub/docs/manual-ocaml/manual034.html (4.00.1) *)

#if OCAML_VERSION >= "4.08"
  let n s = {printed_name = s}
#else
  let n s = s
#endif

let mktype name ?(params=[]) ?(def=Otyp_abstract) doc = {
  path = [];
  orig_path = [];
  kind = Type;
  name = name;
  ty = Some (Osig_type (
      { otype_name    = name;
        otype_params  = List.map (fun v -> v,(true,true)) params;
        otype_type    = def;
        otype_private = Asttypes.Public;
  #if OCAML_VERSION >= "4.03"
        otype_immediate = false;
    #if OCAML_VERSION >= "4.04"
        otype_unboxed = false;
    #endif
  #endif
        otype_cstrs   = [] }, Orec_not));
loc_sig = Lazy.from_val Location.none;
  loc_impl = Lazy.from_val Location.none;
  doc = Lazy.from_val (Some doc);
  file = Cmi "*built-in*";
}

let mkvariant name parent params = {
  path = [];
  orig_path = [];
  kind = Variant parent;
  name = name;
  ty = Some (Osig_type (
      { otype_name    = "";
        otype_params  = [];
        otype_type    = (match params with [] -> Otyp_sum []
                                         | l  -> Otyp_tuple l);
        otype_private = Asttypes.Public;
  #if OCAML_VERSION >= "4.03"
        otype_immediate = false ;
    #if OCAML_VERSION >= "4.04"
        otype_unboxed = false;
    #endif
  #endif
        otype_cstrs   = [] }, Orec_not));
  loc_sig = Lazy.from_val Location.none;
  loc_impl = Lazy.from_val Location.none;
  doc = Lazy.from_val None;
  file = Cmi "*built-in*";
}

let mkexn name params doc = {
  path = [];
  orig_path = [];
  kind = Exception;
  name = name;
  ty = Some (Osig_typext ({
        oext_name        = name;
        oext_type_name   = "exn";
        oext_type_params = [];
        oext_args        = params;
        oext_ret_type    = None;
        oext_private     = Asttypes.Public }, Oext_exception));
  loc_sig = Lazy.from_val Location.none;
  loc_impl = Lazy.from_val Location.none;
  doc = Lazy.from_val (Some doc);
  file = Cmi "*built-in*";
}

let mkkwd name = {
  path = [];
  orig_path = [];
  kind = Keyword;
  name = name;
  ty = None;
  loc_sig = Lazy.from_val Location.none;
  loc_impl = Lazy.from_val Location.none;
  doc = Lazy.from_val None;
  file = Cmi "*built-in*";
}

let var name = Otyp_var (false, name)

let constr ?(params=[]) name =
  Otyp_constr (Oide_ident name, List.map var params)

let ibool =
  mktype "bool"
    ~def:(Otyp_sum ["true",[],None; "false",[],None])
    "The type of booleans (truth values)."

let ilist =
  mktype "list"
    ~params:["'a"]
    ~def:(Otyp_sum ["[]", [], None;
                    "::", [var "a"; constr ~params:["a"] (n "list")], None])
    "The type of lists whose elements have type 'a."

let ioption =
  mktype "option"
    ~def:(Otyp_sum ["None",[],None; "Some", [var "a"], None])
    "The type of optional values of type 'a."

let types = [
  mktype "int" "The type of integer numbers.";
  mktype "char" "The type of characters.";
  mktype "string" "The type of character strings.";
  mktype "float" "The type of floating-point numbers.";
  ibool;
  mktype "unit" ~def:(Otyp_stuff "()") "The type of the unit value.";
  mktype "exn" "The type of exception values.";
  mktype "array" "The type of arrays whose elements have type 'a.";
  ilist;
  ioption;
  mktype "int32"
    "The type of signed 32-bit integers. See the Int32 module.";
  mktype "int64"
    "The type of signed 64-bit integers. See the Int64 module.";
  mktype "nativeint"
    "The type of signed, platform-native integers (32 bits on 32-bit \
     processors, 64 bits on 64-bit processors). See the Nativeint module.";
  mktype "format6"
    "The type of format strings. 'a is the type of the parameters of the \
     format, 'f is the result type for the printf-style functions, 'b is the \
     type of the first argument given to %a and %t printing functions (see \
     module Printf), 'c is the result type of these functions, and also the \
     type of the argument transmitted to the first argument of kprintf-style \
     functions, 'd is the result type for the scanf-style functions (see \
     module Scanf), and 'e is the type of the receiver function for the \
     scanf-style functions.";
  mktype "lazy_t"
    "This type is used to implement the Lazy module. It should not be used \
     directly.";
]

let variants = [
  mkvariant "true" ibool [];
  mkvariant "false" ibool [];
  mkvariant "::" ilist [var "a"; constr ~params:["a"] (n "list")];
  mkvariant "[]" ilist [];
  mkvariant "Some" ioption [var "a"];
  mkvariant "None" ioption [];
]

let exceptions = [
  mkexn "Match_failure" [constr (n "string"); constr (n "int"); constr (n "int")]
    "Exception raised when none of the cases of a pattern-matching apply. \
     The arguments are the location of the match keyword in the source code \
     (file name, line number, column number).";
  mkexn "Assert_failure" [constr (n "string"); constr (n "int"); constr (n "int")]
    "Exception raised when an assertion fails. The arguments are the \
     location of the assert keyword in the source code (file name, line \
     number, column number).";
  mkexn "Invalid_argument" [constr (n "string")]
    "Exception raised by library functions to signal that the given \
     arguments do not make sense.";
  mkexn "Failure" [constr (n "string")]
    "Exception raised by library functions to signal that they are undefined \
     on the given arguments.";
  mkexn "Not_found" []
    "Exception raised by search functions when the desired object could not \
     be found.";
  mkexn "Out_of_memory" []
    "Exception raised by the garbage collector when there is insufficient \
     memory to complete the computation.";
  mkexn "Stack_overflow" []
    "Exception raised by the bytecode interpreter when the evaluation stack \
     reaches its maximal size. This often indicates infinite or excessively \
     deep recursion in the user’s program.";
  mkexn "Sys_error" [constr (n "string")]
    "Exception raised by the input/output functions to report an operating \
     system error.";
  mkexn "End_of_file" []
    "Exception raised by input functions to signal that the end of file has \
     been reached.";
  mkexn "Division_by_zero" []
    "Exception raised by integer division and remainder operations when \
     their second argument is zero.";
  mkexn "Sys_blocked_io" []
    "A special case of Sys_error raised when no I/O is possible on a \
     non-blocking I/O channel.";
  mkexn "Undefined_recursive_module"
    [constr (n "string"); constr (n "int"); constr (n "int")]
    "Exception raised when an ill-founded recursive module definition is \
     evaluated. The arguments are the location of the definition in the \
     source code (file name, line number, column number).";
]

let keywords = List.map mkkwd [
    "and"; "as"; "assert"; "begin"; "class"; "constraint"; "do"; "done";
    "downto"; "else"; "end"; "exception"; "external"; "false"; "for"; "fun";
    "function"; "functor"; "if"; "in"; "include"; "inherit"; "inherit!";
    "initializer"; "lazy"; "let"; "match"; "method"; "method!"; "module";
    "mutable"; "new"; "object"; "of"; "open"; "open!"; "private"; "rec";
    "sig"; "struct"; "then"; "to"; "true"; "try"; "type"; "val"; "val!";
    "virtual"; "when"; "while"; "with";
  ]

let all = types @ variants @ exceptions @ keywords
