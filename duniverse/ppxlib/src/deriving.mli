(** Deriving code from type declarations *)

open Import

(** Specification of generator arguments *)
module Args : sig
  type ('a, 'b) t
  type 'a param

  val empty : ('m, 'm) t

  val arg :
    string ->
    (expression, 'a -> 'a option, 'a option) Ast_pattern.t ->
    'a option param

  val flag : string -> bool param
  (** Flag matches punned labelled argument, i.e. of the form [~foo]. It returns
      [true] iff the argument is present. *)

  val ( +> ) : ('m1, 'a -> 'm2) t -> 'a param -> ('m1, 'm2) t

  (** For convenience, so that one can write the following without having to
      open both [Ast_pattern] and [Deriving.Args]:

      {[
        Deriving.Args.(
          empty
          +> arg_option "foo" (estring __)
          +> arg_option "bar" (pack2 (eint __ ** eint __))
          +> flag "dotdotdot")
      ]} *)
  include module type of struct
      include Ast_pattern
    end
    with type ('a, 'b, 'c) t := ('a, 'b, 'c) Ast_pattern.t
end

(** {5 Generator registration} *)

type t
(** Type of registered derivers *)

module Generator : sig
  type deriver = t
  type ('output_ast, 'input_ast) t

  val make :
    ?attributes:Attribute.packed list (* deprecated, unused *) ->
    ?deps:deriver list ->
    ('f, 'output_ast) Args.t ->
    (loc:Location.t -> path:string -> 'input_ast -> 'f) ->
    ('output_ast, 'input_ast) t
  (** [make args gen] creates a generator that can be passed to {!Deriving.add}
      to generate an output AST from an input AST and generator arguments.

      [deps] is a list of derivers that this generator depends on.

      [attributes] is deprecated and unused. It is only kept for backward
      compatibility. *)

  val make_noarg :
    ?attributes:Attribute.packed list (* deprecated, unused *) ->
    ?deps:deriver list ->
    (loc:Location.t -> path:string -> 'input_ast -> 'output_ast) ->
    ('output_ast, 'input_ast) t
  (** Same as {!make}, but without arguments. *)

  module V2 : sig
    val make :
      ?attributes:Attribute.packed list (* deprecated, unused *) ->
      ?deps:deriver list ->
      ('f, 'output_ast) Args.t ->
      (ctxt:Expansion_context.Deriver.t -> 'input_ast -> 'f) ->
      ('output_ast, 'input_ast) t
    (** Same as {!Generator.make}, but the generator has access to an expansion
        context. *)

    val make_noarg :
      ?attributes:Attribute.packed list (* deprecated, unused *) ->
      ?deps:deriver list ->
      (ctxt:Expansion_context.Deriver.t -> 'input_ast -> 'output_ast) ->
      ('output_ast, 'input_ast) t
    (** Same as {!Generator.make_noarg}, but the generator has access to an
        expansion context. *)
  end

  val apply :
    ('output_ast, 'input_ast) t ->
    name:string ->
    ctxt:Expansion_context.Deriver.t ->
    'input_ast ->
    (string * expression) list ->
    'output_ast
end
with type deriver := t

val add :
  ?str_type_decl:(structure, rec_flag * type_declaration list) Generator.t ->
  ?str_type_ext:(structure, type_extension) Generator.t ->
  ?str_exception:(structure, type_exception) Generator.t ->
  ?str_module_type_decl:(structure, module_type_declaration) Generator.t ->
  ?sig_type_decl:(signature, rec_flag * type_declaration list) Generator.t ->
  ?sig_type_ext:(signature, type_extension) Generator.t ->
  ?sig_exception:(signature, type_exception) Generator.t ->
  ?sig_module_type_decl:(signature, module_type_declaration) Generator.t ->
  ?extension:(loc:Location.t -> path:string -> core_type -> expression) ->
  string ->
  t
(** Register a new deriving generator.

    The various arguments are for the various items on which derivers can be
    attached in structure and signatures.

    We distinguish [exception] from [type_extension] as [exception E] is not
    exactly the same as [type exn += E]. Indeed if the type [exn] is redefined,
    then [type exn += E] will add [E] to the new [exn] type while [exception E]
    will add [E] to the predefined [exn] type.

    [extension] register an expander for extension with the name of the deriver.
    This is here mostly to support the ppx_deriving backend. *)

val add_alias :
  string ->
  ?str_type_decl:t list ->
  ?str_type_ext:t list ->
  ?str_exception:t list ->
  ?str_module_type_decl:t list ->
  ?sig_type_decl:t list ->
  ?sig_type_ext:t list ->
  ?sig_exception:t list ->
  ?sig_module_type_decl:t list ->
  t list ->
  t
(** [add_alias name set] add an alias. When the user write the alias, all the
    generator of [set] will be used instead. It is possible to override the set
    for any of the context by passing the specific set in the approriate
    optional argument of [add_alias]. *)

val ignore : t -> unit
(** Ignore a deriver. So that one can write:
    [Deriving.add ... |>
    Deriving.ignore] *)
