(*
  Table of predefined types.
*)

open Ast

let list_def : type_def =
  let loc = dummy_loc in
  (
    loc,
    ("list", ["a"], []),
    List (loc, Tvar (loc, "a"), [])
  )

let option_def : type_def =
  let loc = dummy_loc in
  (
    loc,
    ("option", ["a"], []),
    Option (loc, Tvar (loc, "a"), [])
  )

let nullable_def : type_def =
  let loc = dummy_loc in
  (
    loc,
    ("nullable", ["a"], []),
    Nullable (loc, Tvar (loc, "a"), [])
  )

let shared_def : type_def =
  let loc = dummy_loc in
  (
    loc,
    ("shared", ["a"], []),
    Shared (loc, Tvar (loc, "a"), [])
  )

let wrap_def : type_def =
  let loc = dummy_loc in
  (
    loc,
    ("wrap", ["a"], []),
    Wrap (loc, Tvar (loc, "a"), [])
  )


let list = [
    "unit", 0, None;
    "bool", 0, None;
    "int", 0, None;
    "float", 0, None;
    "string", 0, None;
    "abstract", 0, None;
    "list", 1, Some list_def;
    "option", 1, Some option_def;
    "nullable", 1, Some nullable_def;
    "shared", 1, Some shared_def;
    "wrap", 1, Some wrap_def;
  ]

let make_table () =
  let tbl = Hashtbl.create 20 in
  List.iter (
    fun (k, n, opt_t) ->
      Hashtbl.add tbl k (n, opt_t)
  ) list;
  tbl
