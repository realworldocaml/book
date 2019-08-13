open Atd.Import
open Atd.Ast
open Mapping

type t = (Ocaml.Repr.t, Json.json_repr) Mapping.mapping
type variant_mapping = (Ocaml.Repr.t, Json.json_repr) Mapping.variant_mapping

(*
  Translation of the types into the ocaml/json mapping.
*)

let check_json_sum loc json_sum_param variants =
  if json_sum_param.Json.json_open_enum then (
    let variants_with_arg =
      List.filter (function {var_arg = Some _} -> true | _ -> false) variants
    in
    match variants_with_arg with
    | [] ->
        Error.error loc
          "Missing catch-all case of the form `| Other of string`, \
           required by <json open_enum>."
    | [x] -> (
        match x.var_arg with
        | None -> assert false
        | Some (String _) -> ()
        | Some mapping ->
            let loc = Mapping.loc_of_mapping mapping in
            Error.error loc
              "The argument of this variant must be of type string \
               as imposed by <json open_enum>."
      )
    | _ ->
        Error.error loc
          "Multiple variants have arguments, which doesn't make sense \
           when combined with <json open_enum>."
  )

let rec mapping_of_expr (x : type_expr) =
  match x with
  | Sum (loc, l, an) ->
      let ocaml_t = Ocaml.Repr.Sum (Ocaml.get_ocaml_sum an) in
      let json_sum_param = Json.get_json_sum an in
      let json_t = Json.Sum (Json.get_json_sum an) in
      let variants = List.map mapping_of_variant l in
      check_json_sum loc json_sum_param variants;
      Sum (loc, Array.of_list variants,
           ocaml_t, json_t)

  | Record (loc, l, an) ->
      let ocaml_t = Ocaml.Repr.Record (Ocaml.get_ocaml_record an) in
      let ocaml_field_prefix = Ocaml.get_ocaml_field_prefix an in
      let json_t = Json.Record (Json.get_json_record an) in
      Record (loc,
              Array.of_list
                (List.map (mapping_of_field ocaml_field_prefix) l),
              ocaml_t, json_t)

  | Tuple (loc, l, _) ->
      let ocaml_t = Ocaml.Repr.Tuple in
      let json_t = Json.Tuple in
      Tuple (loc, Array.of_list (List.map mapping_of_cell l),
             ocaml_t, json_t)

  | List (loc, x, an) ->
      let ocaml_t = Ocaml.Repr.List (Ocaml.get_ocaml_list an) in
      let json_t = Json.List (Json.get_json_list an) in
      List (loc, mapping_of_expr x, ocaml_t, json_t)

  | Option (loc, x, _) ->
      let ocaml_t = Ocaml.Repr.Option in
      let json_t = Json.Option in
      Option (loc, mapping_of_expr x, ocaml_t, json_t)

  | Nullable (loc, x, _) ->
      let ocaml_t = Ocaml.Repr.Nullable in
      let json_t = Json.Nullable in
      Nullable (loc, mapping_of_expr x, ocaml_t, json_t)

  | Shared (loc, _, _) ->
      Error.error loc "Sharing is not supported by the JSON interface"

  | Wrap (loc, x, an) ->
      let ocaml_t = Ocaml.Repr.Wrap (Ocaml.get_ocaml_wrap loc an) in
      let json_t = Json.Wrap in
      Wrap (loc, mapping_of_expr x, ocaml_t, json_t)

  | Name (loc, (_, s, l), an) ->
      (match s with
         "unit" ->
           Unit (loc, Unit, Unit)
       | "bool" ->
           Bool (loc, Bool, Bool)
       | "int" ->
           let o = Ocaml.get_ocaml_int an in
           Int (loc, Int o, Int)
       | "float" ->
           let j = Json.get_json_float an in
           Float (loc, Float, Float j)
       | "string" ->
           String (loc, String, String)
       | s ->
           Name (loc, s, List.map mapping_of_expr l, None, None)
      )
  | Tvar (loc, s) ->
      Tvar (loc, s)

and mapping_of_cell (loc, x, an) =
  let default = Ocaml.get_ocaml_default an in
  let doc = Atd.Doc.get_doc loc an in
  let ocaml_t =
    Ocaml.Repr.Cell {
      Ocaml.ocaml_default = default;
      ocaml_fname = "";
      ocaml_mutable = false;
      ocaml_fdoc = doc;
    }
  in
  let json_t = Json.Cell in
  {
    cel_loc = loc;
    cel_value = mapping_of_expr x;
    cel_arepr = ocaml_t;
    cel_brepr = json_t
  }

and mapping_of_variant = function
  | Variant (loc, (s, an), o) ->
      let ocaml_cons = Ocaml.get_ocaml_cons s an in
      let doc = Atd.Doc.get_doc loc an in
      let ocaml_t =
        Ocaml.Repr.Variant {
          Ocaml.ocaml_cons = ocaml_cons;
          ocaml_vdoc = doc;
        }
      in
      let json_cons = Json.get_json_cons s an in
      let json_t =
        Json.Variant {
          Json.json_cons = json_cons;
        }
      in
      let arg = Option.map mapping_of_expr o in
      {
        var_loc = loc;
        var_cons = s;
        var_arg = arg;
        var_arepr = ocaml_t;
        var_brepr = json_t
      }

  | Inherit _ -> assert false

and mapping_of_field ocaml_field_prefix = function
    `Field (loc, (s, fk, an), x) ->
      let fvalue = mapping_of_expr x in
      let ocaml_default, json_unwrapped =
        match fk, Ocaml.get_ocaml_default an with
          Required, None -> None, false
        | Optional, None -> Some "None", true
        | (Required | Optional), Some _ ->
            Error.error loc "Superfluous default OCaml value"
        | With_default, Some s -> Some s, false
        | With_default, None ->
            (* will try to determine implicit default value later *)
            None, false
      in
      let ocaml_fname = Ocaml.get_ocaml_fname (ocaml_field_prefix ^ s) an in
      let ocaml_mutable = Ocaml.get_ocaml_mutable an in
      let doc = Atd.Doc.get_doc loc an in
      let json_fname = Json.get_json_fname s an in
      { f_loc = loc;
        f_name = s;
        f_kind = fk;
        f_value = fvalue;

        f_arepr = Ocaml.Repr.Field {
          Ocaml.ocaml_default = ocaml_default;
          ocaml_fname = ocaml_fname;
          ocaml_mutable = ocaml_mutable;
          ocaml_fdoc = doc;
        };

        f_brepr = Json.Field {
          Json.json_fname;
          json_unwrapped;
        };
      }

  | `Inherit _ -> assert false


let def_of_atd atd =
  Ox_emit.def_of_atd atd ~target:Json ~external_:Json.External
    ~mapping_of_expr ~def:Json.Def

let defs_of_atd_module l =
  List.map (function Atd.Ast.Type def -> def_of_atd def) l

let defs_of_atd_modules l =
  List.map (fun (is_rec, l) -> (is_rec, defs_of_atd_module l)) l

let json_normalizer_of_adapter_path module_ =
  module_ ^ ".normalize"

let json_restorer_of_adapter_path module_ =
  module_ ^ ".restore"
