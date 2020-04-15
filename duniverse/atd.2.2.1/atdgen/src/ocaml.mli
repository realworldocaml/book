(** OCaml-specific options derived from ATD annotations. *)

type pp_convs =
  | Camlp4 of string list
  | Ppx of string list

type atd_ocaml_sum = Classic | Poly
type atd_ocaml_record = Record | Object
type atd_ocaml_int = Int | Char | Int32 | Int64 | Float
type atd_ocaml_list = List | Array
type target = Default | Biniou | Json | Validate | Bucklescript

type atd_ocaml_wrap = {
  ocaml_wrap_t : string;
  ocaml_wrap : string;
  ocaml_unwrap : string;
}

type atd_ocaml_field = {
  ocaml_default : string option;
  ocaml_fname : string;
  ocaml_mutable : bool;
  ocaml_fdoc : Atd.Doc.doc option;
}

type atd_ocaml_variant = {
  ocaml_cons : string;
  ocaml_vdoc : Atd.Doc.doc option;
}

type atd_ocaml_def = {
  ocaml_predef : bool;
  ocaml_ddoc : Atd.Doc.doc option;
}

module Repr : sig
  (** OCaml-specific options that decorate each kind of ATD AST node. *)
  type t =
    | Unit
    | Bool
    | Int of atd_ocaml_int
    | Float
    | String
    | Sum of atd_ocaml_sum
    | Record of atd_ocaml_record
    | Tuple
    | List of atd_ocaml_list
    | Option
    | Nullable
    | Wrap of atd_ocaml_wrap option
    | Name of string
    | External of (string * string * string)
        (*
          (module providing the type,
           module providing everything else,
           type name)
        *)

    | Cell of atd_ocaml_field
    | Field of atd_ocaml_field
    | Variant of atd_ocaml_variant
    | Def of atd_ocaml_def
end

val get_ocaml_sum : target -> Atd.Annot.t -> atd_ocaml_sum
val get_ocaml_record : target -> Atd.Annot.t -> atd_ocaml_record
val get_ocaml_field_prefix : target -> Atd.Annot.t -> string
val get_ocaml_list : target -> Atd.Annot.t -> atd_ocaml_list
val get_ocaml_wrap : type_param:string list -> target -> Atd.Ast.loc ->
  Atd.Annot.t -> atd_ocaml_wrap option
val get_ocaml_int : target -> Atd.Annot.t -> atd_ocaml_int
val get_ocaml_default : target -> Atd.Annot.t -> string option
val get_ocaml_cons : target -> string -> Atd.Annot.t -> string
val get_ocaml_fname : target -> string -> Atd.Annot.t -> string
val get_ocaml_mutable : target -> Atd.Annot.t -> bool
val get_ocaml_predef : target -> Atd.Annot.t -> bool

val get_ocaml_module_and_t
  : target
  -> string
  -> Atd.Annot.t
  -> (string * string * string) option


val get_implicit_ocaml_default
  : (Repr.t, 'b) Mapping.mapping
  -> string option

val unwrap_option
  :  ('b, 'c) Mapping.mapping
  -> ('b, 'c) Mapping.mapping

val ocaml_of_atd
  : ?pp_convs:pp_convs
  -> target:target
  -> type_aliases:string option
  -> (Atd.Ast.loc * Atd.Ast.annot) * (bool * Atd.Ast.module_body) list
  -> string

type create_fields =
  { intf_params: string
  ; impl_params: string
  ; impl_fields: string
  }

val map_record_creator_field
  : ((Repr.t, 'a) Mapping.mapping
     -> (Repr.t, 'b) Mapping.mapping)
  -> (Repr.t, 'a) Mapping.field_mapping
  -> create_fields

val tick : atd_ocaml_sum -> string

val dot : atd_ocaml_record -> string

val obj_unimplemented : Atd.Ast.loc -> atd_ocaml_record -> unit
