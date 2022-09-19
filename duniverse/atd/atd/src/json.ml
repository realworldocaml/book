(*
  Mapping from ATD to JSON
*)

type json_float =
  | Float of int option (* max decimal places *)
  | Int

type ocaml_adapter = {
    normalize : string;
    restore : string;
}

type json_adapter = {
  ocaml_adapter : ocaml_adapter option;
  java_adapter : string option;
}

let no_adapter = {
  ocaml_adapter = None;
  java_adapter = None;
}

type json_list = Array | Object

type json_variant = { json_cons : string }

type json_field = {
  json_fname  : string;           (* <json name=...> *)
  json_unwrapped : bool;
}

type json_record = {
  json_keep_nulls : bool; (* { ... } <json keep_nulls> *)
  json_record_adapter : json_adapter;
}

type json_sum = {
  json_sum_adapter : json_adapter;
  json_open_enum : bool;
  json_lowercase_tags : bool;
}

(*
   Note that json adapters are supported only by records and sums
   at this time.
   TODO: Support json adapters for all kinds of nodes rather than just
   sums and records, preferably without major code duplication.
   Maybe this can be achieved by turning json_repr
   into (json_repr * json_adapter).
*)
type json_repr =
  | Abstract
  | Bool
  | Cell
  | Def
  | External
  | Field of json_field
  | Float of json_float
  | Int
  | List of json_list
  | Nullable
  | Option
  | Record of json_record
  | String
  | Sum of json_sum
  | Tuple
  | Unit
  | Variant of json_variant
  | Wrap (* should we add support for Base64 encoding of binary data? *)

(*
   This must hold all the valid annotations of the form
   '<json ...>'.
*)
let annot_schema_json : Annot.schema = [
  {
    section = "json";
    fields = [
      Type_expr, "adapter.ocaml";
      Type_expr, "adapter.to_ocaml";
      Type_expr, "adapter.from_ocaml";
      Type_expr, "keep_nulls";
      Type_expr, "open_enum";
      Type_expr, "precision";
      Type_expr, "repr";
      Variant, "name";
      Field, "name";
      Field, "tag_field";
    ]
  };
  {
    (* deprecated *)
    section = "ocaml_json";
    fields = [
      Type_def, "from";
      Type_def, "module";
      Type_def, "predef";
      Type_def, "t";
    ]
  };
]

let json_float_of_string s : [ `Float | `Int ] option =
  match s with
      "float" -> Some `Float
    | "int" -> Some `Int
    | _ -> None

let json_precision_of_string s =
  try Some (int_of_string s)
  with _ -> None

let get_json_precision an =
  Annot.get_opt_field
    ~parse:json_precision_of_string
    ~sections:["json"]
    ~field:"precision"
    an

let get_json_float an : json_float =
  match
    Annot.get_field
      ~parse:json_float_of_string
      ~default:`Float
      ~sections:["json"]
      ~field:"repr"
      an
  with
      `Float -> Float (get_json_precision an)
    | `Int -> Int

let json_list_of_string s : json_list option =
  match s with
  | "array" -> Some Array
  | "object" -> Some Object
  | _ -> (* error *) None

(*
   <json adapter.ocaml="Foo.Bar">
   --> { ocaml_adapter =
           Some { normalize="Foo.Bar.normalize" restore="Foo.Bar.restore"};
         java_adapter = None; }
*)
let get_json_adapter an =
  let get_field field =
    Annot.get_opt_field
      ~parse:(fun s -> Some s)
      ~sections:["json"]
      ~field
      an
  in
  let get_loc field = Annot.get_loc_exn ~sections:["json"] ~field an in
  let ocaml_adapter =
    let field_module = "adapter.ocaml" in
    let field_normalize = "adapter.to_ocaml" in
    let field_restore = "adapter.from_ocaml" in
    match
      get_field field_module,
      get_field field_normalize,
      get_field field_restore
    with
    | None, None, None -> None
    | Some m, None, None -> Some { normalize = m ^ ".normalize"; restore = m ^ ".restore"; }
    | None, Some normalize, Some restore ->
      Some { normalize = "(" ^ normalize ^ ")"; restore = "(" ^ restore ^ ")" }
    | Some _, _, _ ->
      Ast.error_at (get_loc field_module)
        (Printf.sprintf "Cannot use %S field together with %S or %S"
           field_module field_normalize field_restore)
    | None, Some _, None ->
      Ast.error_at (get_loc field_normalize)
        (Printf.sprintf "%S is used without required counterpart %S"
           field_normalize field_restore)
    | None, None, Some _ ->
      Ast.error_at (get_loc field_restore)
        (Printf.sprintf "%S is used without required counterpart %S"
           field_restore field_normalize)
  in
  let java_adapter = get_field "adapter.java" in
  { ocaml_adapter;
    java_adapter }

let get_json_open_enum an =
  Annot.get_flag ~sections:["json"] ~field:"open_enum" an

let get_json_lowercase_tags an =
  Annot.get_flag ~sections:["json"] ~field:"lowercase_tags" an

let get_json_sum an = {
  json_sum_adapter = get_json_adapter an;
  json_open_enum = get_json_open_enum an;
  json_lowercase_tags = get_json_lowercase_tags an;
}

let get_json_list an =
  Annot.get_field
    ~parse:json_list_of_string
    ~default:Array
    ~sections:["json"]
    ~field:"repr"
    an

let get_json_cons default an =
  Annot.get_field
    ~parse:(fun s -> Some s)
    ~default
    ~sections:["json"]
    ~field:"name"
    an

let get_json_fname default an =
  Annot.get_field
    ~parse:(fun s -> Some s)
    ~default
    ~sections:["json"]
    ~field:"name"
    an

let get_json_keep_nulls an =
  Annot.get_flag
    ~sections:["json"]
    ~field:"keep_nulls"
    an

let get_json_record an =
  {
    json_keep_nulls = get_json_keep_nulls an;
    json_record_adapter = get_json_adapter an;
  }

