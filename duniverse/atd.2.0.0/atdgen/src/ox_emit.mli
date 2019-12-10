(** Utilities for writing OCaml code generators from a decorated ATD AST. *)

type 'a expr = (Ocaml.Repr.t, 'a) Mapping.mapping
type 'a def = (Ocaml.Repr.t, 'a) Mapping.def
type 'a grouped_defs = (bool * 'a def list) list

type target =
  | Files of string
  | Stdout

val get_full_type_name : (_, _) Mapping.def -> string

val is_exportable : (_, _) Mapping.def -> bool

val make_record_creator
  : ((Ocaml.Repr.t, 'a) Mapping.mapping
     -> (Ocaml.Repr.t, 'b) Mapping.mapping)
  -> (Ocaml.Repr.t, 'a) Mapping.def
  -> string * string

val opt_annot : string option -> string -> string

val opt_annot_def : string option -> string -> string

val insert_annot : string option -> string

val get_type_constraint
  : original_types:(string, string * int) Hashtbl.t
  -> ('a, 'b) Mapping.def
  -> string

val is_function : Indent.t list -> bool

val needs_type_annot : _ expr -> bool

val check : _ grouped_defs -> unit

val write_ocaml : target -> string -> string -> unit

val name_of_var : string -> string

val nth : string -> int -> int -> string

val get_let : is_rec:bool -> is_first:bool -> string * string

val write_opens : Buffer.t -> string list -> unit

val def_of_atd
  : Atd.Ast.loc
    * (string * string list * Atd.Annot.t)
    * Atd.Ast.type_expr
  -> target:Ocaml.target
  -> def:'a
  -> external_:'a
  -> mapping_of_expr:(Atd.Ast.type_expr -> (Ocaml.Repr.t, 'a) Mapping.mapping)
  -> (Ocaml.Repr.t, 'a) Mapping.def

val maybe_write_creator_impl
  : with_create:bool
  -> ((Ocaml.Repr.t, 'a) Mapping.mapping ->
      (Ocaml.Repr.t, 'b) Mapping.mapping)
  -> Buffer.t
  -> ('c * (Ocaml.Repr.t, 'a) Mapping.def list) list
  -> unit

val maybe_write_creator_intf
  : with_create:bool
  -> ((Ocaml.Repr.t, 'a) Mapping.mapping ->
      (Ocaml.Repr.t, 'b) Mapping.mapping)
  -> Buffer.t
  -> (Ocaml.Repr.t, 'a) Mapping.def
  -> unit

val default_value
  : (Ocaml.Repr.t, 'a) Mapping.field_mapping
  -> ((Ocaml.Repr.t, 'a) Mapping.mapping -> (Ocaml.Repr.t, 'b) Mapping.mapping)
  -> string option

val include_intf : (Ocaml.Repr.t, 'a) Mapping.def -> bool
