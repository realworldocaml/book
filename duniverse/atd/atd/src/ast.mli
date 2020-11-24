(** Abstract syntax tree (AST) representing ATD data *)

type loc = Lexing.position * Lexing.position
    (** A location in the source code. *)

exception Atd_error of string
    (** Exception raised by functions of the [atd] library and indicating
        errors. *)

type annot = annot_section list
    (** An annotation, consisting of a sequence of sections.
        {!Annot} provides utilities for handling annotations. *)

and annot_section = string * (loc * annot_field list)
    (** represents a single annotation within edgy brackets.
        [<"foo" bar baz="123" path.to
                                .thing="abc">] in ATD syntax
        translates to:
{v
("foo", (loc1, [ ("bar", (loc2, None));
                 ("baz", (loc3, Some "123"));
                 ("path.to.thing", (loc4, Some "abc")) ] ))
v}
    *)

and annot_field = string * (loc * string option)
    (** An annotation field,
        i.e. a key with an optional value within an annotation. *)


type full_module = module_head * module_body
    (** Contents of an ATD file. *)

and module_head = loc * annot
    (** The head of an ATD file is just a list of annotations. *)

and module_body = module_item list
    (** The body of an ATD file is a list of type definitions.
        Type definitions are implicitely mutually
        recursive. They can be sorted based on dependencies
        using {!Atd.Util.tsort}.
    *)

and module_item =
  | Type of type_def
      (** There is currently only one kind of module items,
          that is single type definitions. *)

and type_def = loc * (string * type_param * annot) * type_expr
    (** A type definition. *)

and type_param = string list
    (** List of type variables without the tick. *)

and type_expr =
    | Sum of loc * variant list * annot
    | Record of loc * field list * annot
    | Tuple of loc * cell list * annot
    | List of loc * type_expr * annot
    | Option of loc * type_expr * annot
    | Nullable of loc * type_expr * annot
    | Shared of loc * type_expr * annot
    | Wrap of loc * type_expr * annot
    | Name of loc * type_inst * annot
    | Tvar of loc * string
      (**
         A type expression is one of the following:
         - [Sum]: a sum type (within square brackets)
         - [Record]: a record type (within curly braces)
         - [Tuple]: a tuple (within parentheses)
         - [List]: a list type written [list] with its parameter
         e.g. [int list]
         - [Option]: an option type written [option] with its parameter
         e.g. [string option]
         - [Nullable]: adds a null value to a type.
         [Option] should be preferred over [Nullable] since
         it makes it possible to distinguish [Some None] from [None].
         - [Shared]: values for which sharing must be preserved. Such
         type expressions may not be parametrized. Values may only
         be shared if the source location of the type expression is the same.
         - [Wrap]: optional wrapping of a type. For example, a timestamp
         represented as a string can be wrapped within a proper time type.
         In that case, the wrapper would parse the timestamp and convert
         it into the internal representation of its choice. Unwrapping
         would consist in converting it back to a string.
         - [Name]: a type name other than [list] or [option], including
         the predefined types [unit], [bool], [int], [float], [string]
         and [abstract].
         - [Tvar]: a type variable identifier without the tick
      *)

and type_inst = loc * string * type_expr list
    (** A type name and its arguments *)

and variant =
  | Variant of loc * (string * annot) * type_expr option
  | Inherit of loc * type_expr
  (**
     A single variant or an [inherit] statement.
     [Inherit] statements can be expanded into variants
     using {!Atd_inherit}
     or at loading time using the [inherit_variant] option
     offered by the {!Atd.Util} functions.
  *)

and cell = loc * type_expr * annot
    (** Tuple cell. Note that annotations placed before the type
        expression are supported and represented here,
        such as the third cell in
        [(float * float * <ocaml default="0.0"> float)]. *)

and field_kind =
  | Required
  | Optional
  | With_default
  (**
     Different kinds of record fields based on the
     - [Required]: required field, e.g. [id : string]
     - [Optional]: optional field without a default value, e.g.
     [?name : string option].  The ATD type of the field
     value must be an option type.
     - [With_default]: optional field with a default value, e.g.
     [~websites : string list]. The default value may be implicit
     or specified explicitely using annotations.
     Each target language that cannot omit fields
     may have to specify the default in its own syntax.

     Sample ATD file:
{v
type level = [ Beginner | Advanced | Expert ]

type user = \{
  id : string;

  ?name : string option;
    (* Field may be omitted when no value is set, if permitted
       by the target language. *)

  ~websites : string list;
    (* Implicit default: empty list.
       Field may be omitted if the field value is
       equal to the default value and the
       target language permits it. *)

  ~level <ocaml default="`Beginner"> : level;
    (* Explicit default for `ocaml'.
       For instance there is no `json' annotation because
       the default for undefined `JSON' fields would be to omit them. *)
}
v}
      *)

and field =
    [ `Field of (loc * (string * field_kind * annot) * type_expr)
    | `Inherit of (loc * type_expr) ]
      (**
         A single record field or an [inherit] statement.
         [`Inherit] statements can be expanded into fields using {!Atd_inherit}
         or at loading time using the [inherit_fields] option
         offered by the {!Atd.Util} functions.
      *)


val loc_of_type_expr : type_expr -> loc
  (** Extract the source location of any type expression. *)

val set_type_expr_loc : loc -> type_expr -> type_expr
  (** Replace the location of the given expression.
      This is a shallow substitution. Sub-expressions are not affected. *)

val string_of_loc : loc -> string
  (** Convert a location into a human-readable string
      such as [File "foo.atd", line 123, characters 40-45]. *)

val error : string -> 'a
  (** [error s] is a shorthand for [raise (Atd_error s)]. *)

val error_at : loc -> string -> 'a
  (** [error_at loc s] raises [Atd_error s'] where [s']
      is the location followed by [s]. *)


val dummy_loc : loc
  (**
     Dummy value for predefined constructs that are
     not associated with a useful source location.
     Should not show up in error messages.
  *)

val annot_of_type_expr : type_expr -> annot
  (**
     Return the annotations associated with a type expression.
     Note that there can be annotations in a variety of places,
     not just after type expressions.
  *)

val map_annot : (annot -> annot) -> type_expr -> type_expr
  (**
     Replacement of the annotations associated with a type
     expression.
     This is a shallow transformation. Sub-expressions are not affected.
  *)

val map_all_annot : (annot -> annot) -> full_module -> full_module
  (**
     Replacement of all annotations occurring in an ATD module.
  *)

val fold : (type_expr -> 'a -> 'a) -> type_expr -> 'a -> 'a
  (**
     Iteration and accumulation over each [type_expr] node
     within a given [type_expr].
  *)

val extract_type_names :
  ?ignorable : string list ->
  type_expr -> string list
  (**
     Extract all the type names occurring in a type expression
     under [`Name], without duplicates.
     @param ignorable specifies a list of type names to exclude from the result
  *)

val is_parametrized : type_expr -> bool
  (**
     Test whether a type expression contains type variables ([`Tvar]).
  *)

val is_required : field_kind -> bool
