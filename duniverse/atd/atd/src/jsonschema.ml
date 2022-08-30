(*
   Translate ATD to JSON Schema (JSS)

   https://json-schema.org/draft/2020-12/json-schema-core.html

   The translation is done in two passes:
   1. Translation to an AST that models the constructs offered by JSON
      Schema.
   2. Translation of that AST to the JSON AST, which is not completely
      straightforward.
*)

open Printf
open Ast

type version =
  | Draft_2019_09
  | Draft_2020_12

(* Change this to the latest version whenever a new version becomes
   available, as promised by --help. *)
let default_version = Draft_2020_12

type json = Yojson.Safe.t

(* optional description field *)
type opt_descr = (string * json) list

type type_expr =
  | Ref of string
  | Null
  | Boolean
  | Integer
  | Number
  | String
  | Array of type_expr
  | Tuple of type_expr list (* a variation on 'array' *)
  | Object of object_
  | Map of type_expr
  | Union of type_expr list
  | Nullable of type_expr
  | Const of json * opt_descr
  | Any

and object_ = {
  properties: property list;
  required: string list; (* list of the properties that are required *)
  xprop: bool; (* whether to allow extra fields; default is true *)
}

and property = string * type_expr * opt_descr

type def = {
  name: string;
  description: string option;
  type_expr: type_expr;
}

(* The root of a JSON Schema *)
type t = {
  schema: string;
  root_def: def;
  defs: def list;
}

let make_id type_name =
  "#/definitions/" ^ type_name

let trans_description_simple loc an =
  match Doc.get_doc loc an with
  | None -> None
  | Some blocks -> Some (Doc.print_text blocks)

let trans_description loc an =
  match trans_description_simple loc an with
  | None -> []
  | Some doc -> ["description", `String doc]

let trans_type_expr ~xprop (x : Ast.type_expr) : type_expr =
  let rec trans_type_expr (x : Ast.type_expr) : type_expr =
    match x with
    | Sum (loc, vl, an) ->
        Union (List.map (fun x ->
          match (x : variant) with
          | Variant (loc, (name, an), opt_e) ->
              let json_name = Json.get_json_cons name an in
              let descr = trans_description loc an in
              (match opt_e with
               | None -> Const (`String json_name, descr)
               | Some e ->
                   Tuple [
                     Const (`String json_name, descr);
                     trans_type_expr e;
                   ]
              )
          | Inherit _ -> assert false
        ) vl)
    | Record (loc, fl, an) ->
        let fields =
          List.map (fun (x : field) ->
            match x with
            | `Field ((loc, (name, kind, an), e) : simple_field) ->
                let json_name = Json.get_json_fname name an in
                let required =
                  match kind with
                  | Required -> Some json_name
                  | Optional
                  | With_default -> None
                in
                let unwrapped_e =
                  match kind, e with
                  | Optional, Option (loc, e, an) -> e
                  | _, e -> e
                in
                let descr = trans_description loc an in
                ((json_name, trans_type_expr unwrapped_e, descr), required)
            | `Inherit _ -> assert false
          ) fl
        in
        let properties = List.map fst fields in
        let required =
          List.filter_map (fun (_, required) -> required) fields
        in
        Object {
          properties;
          required;
          xprop;
        }
    | Tuple (loc, tl, an) ->
        Tuple (List.map (fun (loc, e, an) -> trans_type_expr e) tl)
    | List (loc, e, an) ->
        let json_repr = Json.get_json_list an in
        (match e, json_repr with
         | _, Array ->
             Array (trans_type_expr e)
         | Tuple (loc, [(_, Name (_, (_, "string", _), _), _);
                        (_, value, _)], an2), Object ->
             Map (trans_type_expr value)
         | _, Object ->
             error_at loc
               "This type expression is not of the form (string * _) list. \
                It can't be represented as a JSON object."
        )
    | Option (loc, e, an) ->
        (* usually not what the user intended *)
        let transpiled = Sum (loc, [
          Variant (loc, ("Some", []), Some e);
          Variant (loc, ("None", []), None);
        ], an)
        in
        trans_type_expr transpiled
    | Nullable (loc, e, an) ->
        Nullable (trans_type_expr e)
    | Shared (loc, e, an) -> error_at loc "unsupported: shared"
    | Wrap (loc, e, an) -> trans_type_expr e
    | Tvar (loc, name) -> error_at loc "unsupported: parametrized types"
    | Name (loc, (loc2, name, args), a) ->
        (match name with
         | "unit" -> Null
         | "bool" -> Boolean
         | "int" -> Integer
         | "float" -> Number
         | "string" -> String
         | "abstract" -> Any
         | _ -> Ref (make_id name)
        )
  in
  trans_type_expr x

let trans_item
    ~xprop
    (Type (loc, (name, param, an), e) : module_item) : def =
  if param <> [] then
    error_at loc "unsupported: parametrized types";
  let description = trans_description_simple loc an in
  {
    name;
    description;
    type_expr = trans_type_expr ~xprop e;
  }

let trans_full_module
    ~version
    ~xprop
    ~src_name
    ~root_type
    ((head, body) : full_module) : t =
  let defs = List.map (trans_item ~xprop) body in
  let root_defs, defs = List.partition (fun x -> x.name = root_type) defs in
  let root_def =
    match root_defs with
    | [x] -> x
    | [] ->
        failwith
          (sprintf "Cannot find definition for the requested root type '%s'"
             root_type)
    | _ ->
        failwith (sprintf "Found multiple definitions for type '%s'" root_type)
  in
  let description =
    (* We have 3 kinds of documentation to fit in a single 'description'
       field:
       - "translated by atdcat"
       - description of the whole module
       - description of the root type
    *)
    let loc, an = head in
    let auto_comment = sprintf "Translated by atdcat from %s." src_name in
    [
      Some auto_comment;
      trans_description_simple loc an;
      root_def.description
    ]
    |> List.filter_map (fun x -> x)
    |> String.concat "\n\n"
  in
  let root_def = { root_def with description = Some description } in
  let version_url =
    match version with
    | Draft_2019_09 -> "https://json-schema.org/draft/2019-09/schema"
    | Draft_2020_12 -> "https://json-schema.org/draft/2020-12/schema"
  in
  {
    schema = version_url;
    root_def = root_def;
    defs;
  }

(***************************************************************************)
(* Translation to JSON (because we don't have atdgen :-/ *)
(***************************************************************************)

let string s = `String s

(* optional field *)
let opt field_name f x =
  match x with
  | None -> []
  | Some x -> [field_name, f x]

let make_type_property ~is_nullable name =
  if is_nullable then
    ("type", `List [ `String name; `String "null" ])
  else
    ("type", `String name)

let rec type_expr_to_assoc ?(is_nullable = false) ~version (x : type_expr)
  : (string * json) list =
  match x with
  | Ref s ->
      [ "$ref", `String s ]
  | Null ->
      [ make_type_property ~is_nullable:false "null" ]
  | Boolean ->
      [ make_type_property ~is_nullable "boolean" ]
  | Integer ->
      [ make_type_property ~is_nullable "integer" ]
  | Number ->
      [ make_type_property ~is_nullable "number" ]
  | String ->
      [ make_type_property ~is_nullable "string" ]
  | Any ->
      []
  | Array x ->
      [
        make_type_property ~is_nullable "array";
        "items", type_expr_to_json ~version x
      ]
  | Tuple xs ->
      let shared =
        [
          make_type_property ~is_nullable "array";
          "minItems", `Int (List.length xs);
        ]
      in
      let tail =
        match version with
        | Draft_2020_12 ->
            [
              "items", `Bool false;
              "prefixItems", `List (List.map (type_expr_to_json ~version) xs);
            ]
        | Draft_2019_09 ->
            [
              "additionalItems", `Bool false;
              "items", `List (List.map (type_expr_to_json ~version) xs);
            ]
      in
      shared @ tail
  | Object x ->
      let properties =
        List.map (fun (name, x, descr) ->
          (name, type_expr_to_json ~descr ~version x)
        ) x.properties
      in
      let additional_properties =
        if x.xprop then [] (* true *)
        else [ "additionalProperties", `Bool false ]
      in
      List.flatten [
        [make_type_property ~is_nullable "object"];
        ["required", `List (List.map string x.required)];
        additional_properties;
        ["properties", `Assoc properties];
      ]
  | Map x ->
      [
        make_type_property ~is_nullable "object";
        "additionalProperties", type_expr_to_json ~version x
      ]
  | Union xs ->
      [ "oneOf", `List (List.map (type_expr_to_json ~is_nullable ~version) xs) ]
  | Nullable x ->
      type_expr_to_assoc ~is_nullable:true ~version x
  | Const (json, descr) ->
      descr @ [ "const", json ]

and type_expr_to_json
    ?(is_nullable = false)
    ?(descr = [])
    ~version
    (x : type_expr) : json =
  `Assoc (
    descr @ type_expr_to_assoc ~is_nullable ~version x
  )

let def_to_json ~version (x : def) =
  x.name, `Assoc (
    List.flatten [
      opt "description" string x.description;
      type_expr_to_assoc ~version x.type_expr;
    ]
  )

let to_json ~version (x : t) : json =
  let root_def =
    match def_to_json ~version x.root_def with
    | _, `Assoc fields ->
        (* json-schema-to-typescript (command json2ts) will use the 'title'
           field to generate a name for the root type since otherwise it
           doesn't have a name. That's why we populate the 'title' field
           like this.
           This probably interferes with other tools that expect an actual
           title. Note that the documentation extracted from the
           ATD <doc ...> header goes into 'description', not 'title'.
        *)
        ("title", `String x.root_def.name) :: fields
    | _ -> assert false
  in
  `Assoc (
    ("$schema", `String x.schema)
    :: root_def
    @ ["definitions",
       `Assoc (List.map (fun x -> def_to_json ~version x) x.defs)]
  )

let annot_schema =
  Json.annot_schema_json @ Doc.annot_schema

let print
    ?(version = default_version)
    ?(xprop = true)
    ~src_name ~root_type oc ast =
  ast
  |> trans_full_module ~version ~xprop ~src_name ~root_type
  |> to_json ~version
  |> Yojson.Safe.pretty_to_channel oc;
  output_char oc '\n'
