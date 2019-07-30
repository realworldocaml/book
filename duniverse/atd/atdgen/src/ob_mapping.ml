open Atd.Import
open Atd.Ast
open Mapping

type ob_mapping =
  (Ocaml.Repr.t, Biniou.biniou_repr) Mapping.mapping

(*
  Translation of the types into the ocaml/biniou mapping.
*)

let rec mapping_of_expr (x : type_expr) : ob_mapping =
  match x with
    Sum (loc, l, an) ->
      let ocaml_t = Ocaml.Repr.Sum (Ocaml.get_ocaml_sum an) in
      let biniou_t = Biniou.Sum in
      Sum (loc, Array.of_list (List.map mapping_of_variant l),
           ocaml_t, biniou_t)

  | Record (loc, l, an) ->
      let ocaml_t = Ocaml.Repr.Record (Ocaml.get_ocaml_record an) in
      let ocaml_field_prefix = Ocaml.get_ocaml_field_prefix an in
      let biniou_t = Biniou.Record in
      Record (loc,
              Array.of_list
                (List.map (mapping_of_field ocaml_field_prefix) l),
              ocaml_t, biniou_t)

  | Tuple (loc, l, _) ->
      let ocaml_t = Ocaml.Repr.Tuple in
      let biniou_t = Biniou.Tuple in
      Tuple (loc, Array.of_list (List.map mapping_of_cell l),
             ocaml_t, biniou_t)

  | List (loc, x, an) ->
      let ocaml_t = Ocaml.Repr.List (Ocaml.get_ocaml_list an) in
      let biniou_t = Biniou.List (Biniou.get_biniou_list an) in
      List (loc, mapping_of_expr x, ocaml_t, biniou_t)

  | Option (loc, x, _) ->
      let ocaml_t = Ocaml.Repr.Option in
      let biniou_t = Biniou.Option in
      Option (loc, mapping_of_expr x, ocaml_t, biniou_t)

  | Nullable (loc, x, _) ->
      let ocaml_t = Ocaml.Repr.Nullable in
      let biniou_t = Biniou.Nullable in
      Nullable (loc, mapping_of_expr x, ocaml_t, biniou_t)

  | Shared (_, _, _) ->
      failwith "Sharing is no longer supported"

  | Wrap (loc, x, a) ->
      let ocaml_t = Ocaml.Repr.Wrap (Ocaml.get_ocaml_wrap loc a) in
      let json_t = Biniou.Wrap in
      Wrap (loc, mapping_of_expr x, ocaml_t, json_t)

  | Name (loc, (_, s, l), an) ->
      (match s with
         "unit" ->
           Unit (loc, Unit, Biniou.Unit)
       | "bool" ->
           Bool (loc, Bool, Biniou.Bool)
       | "int" ->
           let o = Ocaml.get_ocaml_int an in
           let b = Biniou.get_biniou_int an in
           Int (loc, Int o, Biniou.Int b)
       | "float" ->
           let b = Biniou.get_biniou_float an in
           Float (loc, Float, Biniou.Float b)
       | "string" ->
           String (loc, String, Biniou.String)
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
  let biniou_t = Biniou.Cell in
  {
    cel_loc = loc;
    cel_value = mapping_of_expr x;
    cel_arepr = ocaml_t;
    cel_brepr = biniou_t
  }


and mapping_of_variant = function
    Variant (loc, (s, an), o) ->
      let ocaml_cons = Ocaml.get_ocaml_cons s an in
      let doc = Atd.Doc.get_doc loc an in
      let ocaml_t =
        Ocaml.Repr.Variant {
          Ocaml.ocaml_cons = ocaml_cons;
          ocaml_vdoc = doc;
        }
      in
      let biniou_t = Biniou.Variant in
      let arg = Option.map mapping_of_expr o in
      {
        var_loc = loc;
        var_cons = s;
        var_arg = arg;
        var_arepr = ocaml_t;
        var_brepr = biniou_t
      }

  | Inherit _ -> assert false

and mapping_of_field ocaml_field_prefix = function
    `Field (loc, (s, fk, an), x) ->
      let fvalue = mapping_of_expr x in
      let ocaml_default, biniou_unwrapped =
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

        f_brepr = Biniou.Field { Biniou.biniou_unwrapped = biniou_unwrapped };
      }

  | `Inherit _ -> assert false


let def_of_atd atd =
  Ox_emit.def_of_atd atd ~target:Biniou ~external_:Biniou.External
    ~mapping_of_expr ~def:Biniou.Def

let defs_of_atd_module l =
  List.map (function Atd.Ast.Type def -> def_of_atd def) l

let defs_of_atd_modules l =
  List.map (fun (is_rec, l) -> (is_rec, defs_of_atd_module l)) l
