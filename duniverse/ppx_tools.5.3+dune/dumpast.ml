(*  This file is part of the ppx_tools package.  It is released  *)
(*  under the terms of the MIT license (see LICENSE file).       *)
(*  Copyright 2013  Alain Frisch and LexiFi                      *)

(* Illustrate how to use AST lifting to create a pretty-printer *)

open Outcometree

let locs = ref (`Discard : [`Discard|`Underscore|`Keep])
let attrs = ref (`Discard_empty : [`Discard|`Underscore|`Keep|`Discard_empty])

class out_value_builder =
  object
    method record (_ty : string) x =
      let x =
        List.filter (function (_, Oval_ellipsis) -> false | _ -> true) x
      in
      let f (l, s) = Oide_ident { printed_name = l }, s in
      Oval_record (List.map f x)
    method constr (_ty : string) (c, args) =
        Oval_constr (Oide_ident { printed_name = c }, args)
    method list x = Oval_list x
    method array x = Oval_list (Array.to_list x)
    method tuple x = Oval_tuple x
    method int x = Oval_int x
    method string x = Oval_string (x, max_int, Ostr_string)
    method char x = Oval_char x
    method int32 x = Oval_int32 x
    method int64 x = Oval_int64 x
    method nativeint x = Oval_nativeint x
  end

let lift =
  object
    inherit [_] Ast_lifter.lifter as super
    inherit out_value_builder
    method! lift_Location_t l =
      match !locs with
      | `Discard -> Oval_ellipsis
      | `Underscore -> Oval_stuff "_"
      | `Keep -> super # lift_Location_t l
    method! lift_Parsetree_attributes l =
      match !attrs, l with
      | `Discard, _ | `Discard_empty, [] -> Oval_ellipsis
      | `Underscore, _ -> Oval_stuff "_"
      | `Keep, _ | (`Discard_empty, _ :: _) ->
          super # lift_Parsetree_attributes l
  end

let show lifter parse s =
  let v = lifter (parse (Lexing.from_string s)) in
  Format.printf "%s@.==>@.%a@.=========@." s !Oprint.out_value v

let show_expr = show (lift # lift_Parsetree_expression) Parse.expression
let show_pat = show (lift # lift_Parsetree_pattern) Parse.pattern
let show_typ = show (lift # lift_Parsetree_core_type) Parse.core_type

let show_file fn =
  Compenv.readenv Format.err_formatter (Compenv.Before_compile fn);
  let v =
    if Filename.check_suffix fn ".mli" then
      let ast = Pparse.parse_interface ~tool_name:"ocamlc" fn in
      lift # lift_Parsetree_signature ast
    else if Filename.check_suffix fn ".ml" then
      let ast = Pparse.parse_implementation ~tool_name:"ocamlc" fn in
      lift # lift_Parsetree_structure ast
    else
      failwith (Printf.sprintf "Don't know what to do with file %s" fn)
  in
  Format.printf "%s@.==>@.%a@.=========@." fn !Oprint.out_value v

let args =
  let open Arg in
  [
    "-e", String show_expr,
    "<expr> Dump AST for expression <expr>.";

    "-p", String show_pat,
    "<pat> Dump AST for pattern <pat>.";

    "-t", String show_typ,
    "<typ> Dump AST for type expression <typ>.";

    "-loc_discard", Unit (fun () -> locs := `Discard),
    "  Discard location fields. (default)";

    "-loc_underscore", Unit (fun () -> locs := `Underscore),
    "  Display '_' for location fields";

    "-loc_keep", Unit (fun () -> locs := `Keep),
    "  Display real value of location fields";

    "-attrs_discard_empty", Unit (fun () -> attrs := `Discard_empty),
    "  Discard empty attribute fields. (default)";

    "-attrs_discard", Unit (fun () -> attrs := `Discard),
    "  Discard all attribute fields.";

    "-attrs_underscore", Unit (fun () -> attrs := `Underscore),
    "  Display '_' for attribute fields";

    "-attrs_keep", Unit (fun () -> attrs := `Keep),
    "  Display real value of attribute fields";

    "-pp", Arg.String (fun s -> Clflags.preprocessor := Some s),
    "<command>  Pipe sources through preprocessor <command>";

    "-ppx", Arg.String (fun s -> Compenv.first_ppx := s :: !Compenv.first_ppx),
    "<command>  Pipe abstract syntax trees through preprocessor <command>";
  ]


let usage =
  Printf.sprintf "%s [options] [.ml/.mli files]\n" Sys.argv.(0)

let () =
  Compenv.readenv Format.err_formatter Compenv.Before_args;
  try Arg.parse (Arg.align args) show_file usage
  with exn ->
    Errors.report_error Format.err_formatter exn;
    exit 2
