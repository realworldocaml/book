include Ppxlib_ast
include Ast
open Ast_helper

let loc =
  (* This is fine, because the location info is thrown away when the generated code
     written out to the .ml file *)
  Location.none

let lident x = Longident.Lident x

module Loc = struct
  let mk     x = { Location.loc; txt = x }
  let lident x = mk (Longident.parse x) [@@warning "-3"]
end

module List = Stdppx.List
module String = Stdppx.String
module Array = Stdppx.Array

let evar v = Exp.ident (Loc.lident v)
let pvar v = Pat.var (Loc.mk v)

let common_prefix l =
  match l with
  | [] -> ""
  | x :: l ->
    match String.index x '_' with
    | i ->
      let plen = i + 1 in
      let prefix = String.sub x ~pos:0 ~len:plen in
      let has_prefix s =
        String.length s >= plen && String.sub s ~pos:0 ~len:plen = prefix
      in
      if List.for_all l ~f:has_prefix then
        prefix
      else
        ""
    | exception _ -> ""
;;

let map_keyword = function
  | "open"
  | "private"
  | "downto"
  | "to"
  | "mutable"
  | "rec"
  | "nonrec"
  | "virtual"
  | "type"
  | "mod"
  | "begin"
  | "end" as s -> s ^ "_"
  | s -> s
;;

let function_name_of_path path =
  match path with
  | Lident id -> id
  | _ -> assert false
;;

let without_prefix ~prefix s =
  let plen = String.length prefix in
  String.sub s ~pos:plen ~len:(String.length s - plen)
;;

let function_name_of_id ?(prefix="") id =
  let s = without_prefix ~prefix id in
(*  let prefix =
    if prefix <> "" && (prefix.[0] = 'p' || prefix.[0] = 'P') then
      String.sub prefix ~pos:1 ~len:(String.length prefix - 1)
    else
      prefix
  in*)
  match prefix ^ s with
  | "::" -> "cons"
  | "[]" -> "nil"
  | "true" -> "true_"
  | "false" -> "false_"
  | s -> String.lowercase_ascii s |> map_keyword
;;

let fqn_longident' path s : Longident.t =
  match path with
  | Lident _ -> Lident s
  | Ldot (p, _) -> Ldot (p, s)
  | Lapply _ -> assert false
;;

let fqn_longident path id : Longident.t = fqn_longident' path id

let is_loc = function
  | Lident "loc" -> true
  | _ -> false
;;

let get_types ~filename =
  (* Expand "longident_loc" into "longident loc" as it is preferable for what we do here. *)
  let map = object
    inherit Ast.map as super
    inherit Ppxlib_traverse_builtins.map
    method! core_type_desc = function
      | Ptyp_constr ({ txt = Lident "longident_loc"; loc }, []) ->
        Ptyp_constr ({ txt = Lident "loc"; loc},
                     [Typ.constr ~loc { loc; txt = Lident "longident" } []])
      | ty -> super#core_type_desc ty
  end in
  let ic = open_in_bin filename in
  let lb = Lexing.from_channel ic in
  let st = Parse.implementation lb in
  close_in ic;
  List.map st ~f:(function
    | { pstr_desc = Pstr_type (_, tds); _} -> tds
    | _ -> [])
  |> List.concat
  |> List.map ~f:map#type_declaration
  |> List.map ~f:(fun td ->
    (Lident td.ptype_name.txt, td))
;;

let is_wrapper ~prefix lds =
  match lds with
  | [ { pld_name = { txt = s; _ }
      ; pld_type = { ptyp_desc = Ptyp_constr (p, _); _ }; _ } ]
    when s = prefix ^ "desc" ->
    Some p
  | _ -> None
;;

(* Small metaquotation system *)
module M = struct
  let parse f fmt = Format.kasprintf (fun s -> f (Lexing.from_string s)) fmt

  let expr fmt = parse Parse.expression     fmt
  let patt fmt = parse Parse.pattern        fmt
  let ctyp fmt = parse Parse.core_type      fmt
  let str  fmt = parse Parse.implementation fmt

  let stri fmt =
    Format.kasprintf
      (fun s ->
         match Parse.implementation (Lexing.from_string s) with
         | [x] -> x
         | _ -> assert false)
    fmt
end

(* Antiquotations *)
module A = struct
  let expr = Pprintast.expression
  let patt = Pprintast.pattern
  let ctyp = Pprintast.core_type
  let str  = Pprintast.structure
  let id ppf x = Format.pp_print_string ppf (Longident.flatten x |> String.concat ~sep:".")
end
