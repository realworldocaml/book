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
      List.filter (function {var_arg = Some _; _} -> true | _ -> false) variants
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
      let ocaml_t = Ocaml.Repr.Sum (Ocaml.get_ocaml_sum Json an) in
      let json_sum_param = Json.get_json_sum an in
      let json_t = Json.Sum (Json.get_json_sum an) in
      let variants = List.map mapping_of_variant l in
      check_json_sum loc json_sum_param variants;
      Sum (loc, Array.of_list variants,
           ocaml_t, json_t)

  | Record (loc, l, an) ->
      let ocaml_t = Ocaml.Repr.Record (Ocaml.get_ocaml_record Json an) in
      let ocaml_field_prefix = Ocaml.get_ocaml_field_prefix Json an in
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
      let ocaml_t = Ocaml.Repr.List (Ocaml.get_ocaml_list Json an) in
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
      let ocaml_t =
        Ocaml.Repr.Wrap (Ocaml.get_ocaml_wrap ~type_param:[] Json loc an) in
      let json_t = Json.Wrap in
      Wrap (loc, mapping_of_expr x, ocaml_t, json_t)

  | Name (loc, (_, s, l), an) ->
      (match s with
         "unit" ->
           Unit (loc, Unit, Unit)
       | "bool" ->
           Bool (loc, Bool, Bool)
       | "int" ->
           let o = Ocaml.get_ocaml_int Json an in
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

and mapping_of_cell (cel_loc, x, an) =
  { cel_loc
  ; cel_value = mapping_of_expr x
  ; cel_arepr =
      Ocaml.Repr.Cell
        { Ocaml.ocaml_default = Ocaml.get_ocaml_default Json an
        ; ocaml_fname = ""
        ; ocaml_mutable = false
        ; ocaml_fdoc = Atd.Doc.get_doc cel_loc an
        }
  ; cel_brepr = Json.Cell
  }

and mapping_of_variant = function
  | Inherit _ -> assert false
  | Variant (var_loc, (var_cons, an), o) ->
      { var_loc
      ; var_cons
      ; var_arg = Option.map mapping_of_expr o
      ; var_arepr = Ocaml.Repr.Variant
            { Ocaml.ocaml_cons = Ocaml.get_ocaml_cons Json var_cons an
            ; ocaml_vdoc = Atd.Doc.get_doc var_loc an
            }
      ; var_brepr =
          Json.Variant
            { Json.json_cons = Json.get_json_cons var_cons an
            }
      }

and mapping_of_field ocaml_field_prefix = function
  | `Inherit _ -> assert false
  | `Field (f_loc, (f_name, f_kind, an), x) ->
      let { Ox_mapping.ocaml_default; unwrapped } =
        Ox_mapping.analyze_field Json f_loc f_kind an in
      { f_loc
      ; f_name
      ; f_kind
      ; f_value = mapping_of_expr x
      ; f_arepr = Ocaml.Repr.Field
            { Ocaml.ocaml_default
            ; ocaml_fname =
                Ocaml.get_ocaml_fname Json (ocaml_field_prefix ^ f_name) an
            ; ocaml_mutable = Ocaml.get_ocaml_mutable Json an
            ; ocaml_fdoc = Atd.Doc.get_doc f_loc an
            }
      ; f_brepr = Json.Field
            { Json.json_fname = Json.get_json_fname f_name an
            ; json_unwrapped = unwrapped
            }
      }

let defs_of_atd_modules l ~(target : Ocaml.target)=
  (match target with
   | Json
   | Bucklescript -> ()
   | t -> invalid_arg "target must be json or bucklescript");
  List.map (fun (is_rec, l) ->
    ( is_rec
    , List.map (function Atd.Ast.Type atd ->
        Ox_emit.def_of_atd atd ~target ~external_:Json.External
          ~mapping_of_expr ~def:Json.Def
      ) l
    )
  ) l

let json_normalizer_of_adapter_path module_ =
  module_ ^ ".normalize"

let json_restorer_of_adapter_path module_ =
  module_ ^ ".restore"
