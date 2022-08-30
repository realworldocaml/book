(**
  Mapping from ATD to JSON
*)

type ocaml_adapter = {
    normalize : string;
    restore : string;
}

(** Association between languages and json adapter for that language.
    The specification of each json adapter is language-specific. *)
type json_adapter = {
  ocaml_adapter : ocaml_adapter option;
    (** A module implementing [normalize] and [restore]. *)

  java_adapter : string option;
    (** tbd *)
}

val no_adapter : json_adapter

type json_float =
  | Float of int option (* max decimal places *)
  | Int

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

(** The different kinds of ATD nodes with their json-specific options. *)
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
  | Wrap

val annot_schema_json : Annot.schema

val get_json_list : Annot.t -> json_list

val get_json_float : Annot.t -> json_float

val get_json_cons : string -> Annot.t -> string

val get_json_fname : string -> Annot.t -> string

val get_json_record : Annot.t -> json_record

val get_json_sum : Annot.t -> json_sum
