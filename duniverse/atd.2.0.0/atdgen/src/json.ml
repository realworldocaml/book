(*
  Mapping from ATD to JSON
*)

type json_float =
  | Float of int option (* max decimal places *)
  | Int

type json_adapter = {
  ocaml_adapter : string option;
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

let json_float_of_string s : [ `Float | `Int ] option =
  match s with
      "float" -> Some `Float
    | "int" -> Some `Int
    | _ -> None

let json_precision_of_string s =
  try Some (int_of_string s)
  with _ -> None

let get_json_precision an =
  Atd.Annot.get_opt_field
    ~parse:json_precision_of_string
    ~sections:["json"]
    ~field:"precision"
    an

let get_json_float an : json_float =
  match
    Atd.Annot.get_field
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
   --> { ocaml_adapter = Some "Foo.Bar";
         java_adapter = None; }
*)
let get_json_adapter an =
  let ocaml_adapter =
    Atd.Annot.get_opt_field
      ~parse:(fun s -> Some s)
      ~sections:["json"]
      ~field:"adapter.ocaml"
      an
  in
  let java_adapter =
    Atd.Annot.get_opt_field
      ~parse:(fun s -> Some s)
      ~sections:["json"]
      ~field:"adapter.java"
      an
  in
  { ocaml_adapter;
    java_adapter }

let get_json_open_enum an =
  Atd.Annot.get_flag ~sections:["json"] ~field:"open_enum" an

let get_json_lowercase_tags an =
  Atd.Annot.get_flag ~sections:["json"] ~field:"lowercase_tags" an

let get_json_sum an = {
  json_sum_adapter = get_json_adapter an;
  json_open_enum = get_json_open_enum an;
  json_lowercase_tags = get_json_lowercase_tags an;
}

let get_json_list an =
  Atd.Annot.get_field
    ~parse:json_list_of_string
    ~default:Array
    ~sections:["json"]
    ~field:"repr"
    an

let get_json_cons default an =
  Atd.Annot.get_field
    ~parse:(fun s -> Some s)
    ~default
    ~sections:["json"]
    ~field:"name"
    an

let get_json_fname default an =
  Atd.Annot.get_field
    ~parse:(fun s -> Some s)
    ~default
    ~sections:["json"]
    ~field:"name"
    an

let get_json_keep_nulls an =
  Atd.Annot.get_flag
    ~sections:["json"]
    ~field:"keep_nulls"
    an

let get_json_record an =
  {
    json_keep_nulls = get_json_keep_nulls an;
    json_record_adapter = get_json_adapter an;
  }

let tests = [
]
