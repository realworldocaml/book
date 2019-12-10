(** Pretty-printing of ATD data *)

val default_annot : Ast.annot_section -> Easy_format.t

val format :
  ?annot: (Ast.annot_section -> Easy_format.t) ->
  Ast.full_module -> Easy_format.t
  (** Pretty-printing. Use the functions of the [Easy_format.Pretty]
      module to convert an [Easy_format.t] into a string
      or add it to a channel or buffer.

      @param annot can be used to specify another way of formatting
                   annotations. The default is available as
                   [default_format_annot].
 *)

val string_of_type_name :
  string -> Ast.type_expr list -> Ast.annot -> string
  (** Convert a type name with its arguments and its annotations
      into a string. *)

val string_of_type_expr : Ast.type_expr -> string
