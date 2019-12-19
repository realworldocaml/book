(** Public API of [ppx_deriving] executable. *)

open Parsetree

#if OCAML_VERSION >= (4, 05, 0)
type tyvar = string Location.loc
#else
type tyvar = string
#endif

(** {2 Registration} *)

(** A type of deriving plugins.

    A structure or signature deriving function accepts a list of
    [~options], a [~path] of modules for the type declaration currently
    being processed (with [[]] for toplevel phrases), and a type declaration
    item ([type t = .. and t' = ..]), and returns a list of items to be
    appended after the type declaration item in structure and signature.
    It is invoked by [[\@\@deriving]] annotations.

    A type deriving function accepts a type and returns a corresponding
    derived expression. It is invoked by [[%derive.foo:]] and [[%foo:]]
    annotations. If this function is missing, the corresponding [[%foo:]]
    annotation is ignored.

    The structure and signature deriving functions are invoked in
    the order in which they appear in the source code. *)
type deriver = {
  name : string ;
  core_type : (core_type -> expression) option;
  type_decl_str : options:(string * expression) list -> path:string list ->
                   type_declaration list -> structure;
  type_ext_str : options:(string * expression) list -> path:string list ->
                   type_extension -> structure;
  module_type_decl_str : options:(string * expression) list ->
                          path:string list ->
                          module_type_declaration -> structure;
  type_decl_sig : options:(string * expression) list -> path:string list ->
                   type_declaration list -> signature;
  type_ext_sig : options:(string * expression) list -> path:string list ->
                  type_extension -> signature;
  module_type_decl_sig : options:(string * expression) list ->
                          path:string list ->
                          module_type_declaration -> signature;
}

(** [register deriver] registers [deriver] according to its [name] field. *)
val register : deriver -> unit

(** [add_register_hook hook] adds [hook] to be executed whenever a new deriver
    is registered. *)
val add_register_hook : (deriver -> unit) -> unit

(** [derivers ()] returns all currently registered derivers. *)
val derivers : unit -> deriver list

(** Creating {!deriver} structure. *)
val create :
  string ->
  ?core_type: (core_type -> expression) ->
  ?type_ext_str: (options:(string * expression) list -> path:string list ->
                   type_extension -> structure) ->
  ?type_ext_sig: (options:(string * expression) list -> path:string list ->
                   type_extension -> signature) ->
  ?type_decl_str: (options:(string * expression) list -> path:string list ->
                    type_declaration list -> structure) ->
  ?type_decl_sig: (options:(string * expression) list -> path:string list ->
                    type_declaration list -> signature) ->
  ?module_type_decl_str: (options:(string * expression) list ->
                           path:string list ->
                           module_type_declaration -> structure) ->
  ?module_type_decl_sig: (options:(string * expression) list ->
                           path:string list ->
                           module_type_declaration -> signature) ->
  unit -> deriver

(** [lookup name] looks up a deriver called [name]. *)
val lookup : string -> deriver option

(** {2 Error handling} *)
val raise_errorf : ?sub:Location.error list ->
                   ?loc:Location.t -> ('a, unit, string, 'b) format4 -> 'a

(** [string_of_core_type typ] unparses [typ], omitting any attributes. *)
val string_of_core_type : Parsetree.core_type -> string

(** {2 Option parsing} *)

(** {!Arg} contains convenience functions that extract constants from
    AST fragments, to be used when parsing options or [[\@attributes]]
    attached to types, fields or constructors.

    The [~name] argument is used in error messages and should receive
    the name of the deriving plugin, e.g. ["show"]. *)
module Arg : sig
  (** A type of conversion functions.

      A conversion function of type ['a conv] converts a raw expression into an
      argument of type ['a]. Or returns [Result.Error "error"] if conversion
      fails. *)
  type 'a conv = expression -> ('a, string) Result.result

  (** [expr] returns the input expression as-is. *)
  val expr : expression conv

  (** [bool expr] extracts a boolean constant from [expr], or returns
      [Result.Error "boolean"] if [expr] does not contain a boolean literal. *)
  val bool : bool conv

  (** [int expr] extracts an integer constant from [expr], or returns
      [Result.Error "integer"] if [expr] does not contain an integer literal. *)
  val int : int conv

  (** [string expr] extracts a string constant from [expr], or returns
      [Result.Error "string"] if [expr] does not contain a string literal. *)
  val string : string conv

  (** [char expr] extracts a char constant from [expr], or returns
      [Result.Error "char"] if [expr] does not contain a char literal. *)
  val char : char conv

  (** [enum values expr] extracts a polymorphic variant constant from [expr],
      or returns [Result.Error "one of: `a, `b, ..."] if [expr] does not
      contain a polymorphic variant constructor included in [values]. *)
  val enum : string list -> string conv

  (** [list f expr] extracts a list constant from [expr] and maps every element
      through [f], or returns [Result.Error "list:..."] where [...] is the
      error returned by [f], or returns [Result.Error "list"] if [expr] does
      not contain a list. *)
  val list : 'a conv -> 'a list conv

  (** [get_attr ~deriver conv attr] extracts the expression from [attr] and converts
      it with [conv], raising [Location.Error] if [attr] is not a structure with
      a single expression or [conv] fails; or returns [None] if [attr] is [None].
      The name of the deriving plugin should be passed as [deriver]; it is used
      in error messages.

      Example usage:
      {[
let deriver = "index"
(* ... *)
  let kind =
    match Ppx_deriving.attr ~deriver "kind" pcd_attributes |>
          Ppx_deriving.Arg.(get_attr ~deriver (enum ["flat"; "nested"])) with
    | Some "flat" -> `flat | Some "nested" -> `nested | None -> `default
  in ..
      ]} *)
  val get_attr : deriver:string -> 'a conv -> attribute option -> 'a option

  (** [get_flag ~deriver attr] returns [true] if [attr] is an empty attribute
      or [false] if it is absent, raising [Location.Error] if [attr] is not
      a structure.
      The name of the deriving plugin should be passed as [deriver]; it is used
      in error messages. *)
  val get_flag : deriver:string -> attribute option -> bool

  (** [get_expr ~deriver conv exp] converts expression [exp] with [conv], raising
      [Location.Error] if [conv] fails.
      The name of the deriving plugin should be passed as [deriver]; it is used
      in error messages. *)
  val get_expr : deriver:string -> 'a conv -> expression -> 'a
end

(** {2 Hygiene} *)

(** A [quoter] remembers a set of expressions. *)
type quoter

(** [quoter ()] creates an empty quoter. *)
val create_quoter : unit -> quoter

(** [quote quoter expr] records a pure expression [expr] within [quoter] and
    returns an expression which has the same value as [expr] in the context
    that [sanitize] provides. *)
val quote : quoter:quoter -> expression -> expression

(** [sanitize module_ quoter expr] wraps [expr] in a way that ensures that the
    contents of [module_] and {!Pervasives}, as well as the identifiers in
    expressions returned by [quote] are in scope, and returns the wrapped
    expression. [module_] defaults to {!Ppx_deriving_runtime} if it's not
    provided*)
val sanitize : ?module_:Longident.t -> ?quoter:quoter -> expression -> expression

(** [with_quoter fn] ≡
    [fun fn a -> let quoter = create_quoter () in sanitize ~quoter (fn quoter a)] *)
val with_quoter : (quoter -> 'a -> expression) -> 'a -> expression

(** {2 AST manipulation} *)

(** [expand_path name] returns [name] with the [path] module path prepended,
    e.g. [expand_path ["Foo";"M"] "t"] = ["Foo.M.t"] and [expand_path [] "t"] = ["t"] *)
val expand_path : path:string list -> string -> string

(** [path_of_type_decl ~path type_] returns [path] if [type_] does not have a manifest
    or the manifest is not a constructor, and the module path of manifest otherwise.

    [path_of_type_decl] is useful when determining the canonical path location
    of fields and constructors; e.g. for [type bar = M.foo = A | B], it will return
    [["M"]]. *)
val path_of_type_decl : path:string list -> type_declaration -> string list

(** [mangle_type_decl ~fixpoint affix type_] derives a function name from [type_] name
    by doing nothing if [type_] is named [fixpoint] (["t"] by default), or
    appending and/or prepending [affix] via an underscore. *)
val mangle_type_decl :
   ?fixpoint:string ->
   [ `Prefix of string | `Suffix of string | `PrefixSuffix of string * string ] ->
   type_declaration -> string

(** [mangle_lid ~fixpoint affix lid] does the same as {!mangle_type_decl}, but for
    the last component of [lid]. *)
val mangle_lid : ?fixpoint:string ->
   [ `Prefix of string | `Suffix of string | `PrefixSuffix of string * string] ->
   Longident.t -> Longident.t

(** [attr ~deriver name attrs] searches for an attribute [\[\@deriving.deriver.attr\]]
    in [attrs] if any attribute with name starting with [\@deriving.deriver] exists,
    or [\[\@deriver.attr\]] if any attribute with name starting with [\@deriver] exists,
    or [\[\@attr\]] otherwise. *)
val attr : deriver:string -> string -> attributes -> attribute option

(** [attr_warning expr] builds the attribute [\@ocaml.warning expr] *)
val attr_warning: expression -> attribute

(** [free_vars_in_core_type typ] returns unique free variables in [typ] in
    lexical order. *)
val free_vars_in_core_type : core_type -> tyvar list

(** [remove_pervasives ~deriver typ] removes the leading "Pervasives."
    module name in longidents.
    Type expressions marked with [\[\@nobuiltin\]] are ignored.

    The name of the deriving plugin should be passed as [deriver]; it is used
    in error messages. *)
val remove_pervasives : deriver:string -> core_type -> core_type

(** [fresh_var bound] returns a fresh variable name not present in [bound].
    The name is selected in alphabetical succession. *)
val fresh_var : string list -> string

(** [fold_left_type_decl fn accum type_] performs a left fold over all type variable
    (i.e. not wildcard) parameters in [type_]. *)
val fold_left_type_decl : ('a -> tyvar -> 'a) -> 'a -> type_declaration -> 'a

(** [fold_right_type_decl fn accum type_] performs a right fold over all type variable
    (i.e. not wildcard) parameters in [type_]. *)
val fold_right_type_decl : (tyvar -> 'a -> 'a) -> type_declaration -> 'a -> 'a

(** [fold_left_type_ext fn accum type_] performs a left fold over all type variable (i.e. not
    wildcard) parameters in [type_]. *)
val fold_left_type_ext : ('a -> tyvar -> 'a) -> 'a -> type_extension -> 'a

(** [fold_right_type_ext fn accum type_] performs a right fold over all type variable (i.e. not
    wildcard) parameters in [type_]. *)
val fold_right_type_ext : (tyvar -> 'a -> 'a) -> type_extension -> 'a -> 'a

(** [poly_fun_of_type_decl type_ expr] wraps [expr] into [fun poly_N -> ...] for every
    type parameter ['N] present in [type_]. For example, if [type_] refers to
    [type ('a, 'b) map], [expr] will be wrapped into [fun poly_a poly_b -> [%e expr]].

    [_] parameters are ignored.  *)
val poly_fun_of_type_decl : type_declaration -> expression -> expression

(** Same as {!poly_fun_of_type_decl} but for type extension. *)
val poly_fun_of_type_ext : type_extension -> expression -> expression

(** [poly_apply_of_type_decl type_ expr] wraps [expr] into [expr poly_N] for every
    type parameter ['N] present in [type_]. For example, if [type_] refers to
    [type ('a, 'b) map], [expr] will be wrapped into [[%e expr] poly_a poly_b].

    [_] parameters are ignored. *)
val poly_apply_of_type_decl : type_declaration -> expression -> expression

(** Same as {!poly_apply_of_type_decl} but for type extension. *)
val poly_apply_of_type_ext : type_extension -> expression -> expression

(** [poly_arrow_of_type_decl fn type_ typ] wraps [typ] in an arrow with [fn [%type: 'N]]
    as argument for every type parameter ['N] present in [type_]. For example, if
    [type_] refers to [type ('a, 'b) map] and [fn] is [fun var -> [%type: [%t var] -> string]],
    [typ] will be wrapped into [('a -> string) -> ('b -> string) -> [%t typ]].

    [_] parameters are ignored. *)
val poly_arrow_of_type_decl : (core_type -> core_type) ->
                              type_declaration -> core_type -> core_type

(** Same as {!poly_arrow_of_type_decl} but for type extension. *)
val poly_arrow_of_type_ext : (core_type -> core_type) ->
                              type_extension -> core_type -> core_type

(** [core_type_of_type_decl type_] constructs type [('a, 'b, ...) t] for
    type declaration [type ('a, 'b, ...) t = ...]. *)
val core_type_of_type_decl : type_declaration -> core_type

(** Same as {!core_type_of_type_decl} but for type extension. *)
val core_type_of_type_ext : type_extension -> core_type

(** [instantiate bound type_] returns [typ, vars, bound'] where [typ] is a type
    instantiated from type declaration [type_], [vars] ≡ [free_vars_in_core_type typ]
    and [bound'] ≡ [bound @ vars]. *)
val instantiate : string list -> type_declaration ->
                  core_type * string list * string list

(** [fold_exprs ~unit fn exprs] folds [exprs] using head of [exprs] as initial
    accumulator value, or [unit] if [exprs = []].

    See also {!seq_reduce} and {!binop_reduce}. *)
val fold_exprs : ?unit:expression -> (expression -> expression -> expression) ->
                 expression list -> expression

(** When [sep] is present:
    [seq_reduce] ≡ [fun x a b -> [%expr [%e a]; [%e x]; [%e b]]].
    When [sep] is missing:
    [seq_reduce] ≡ [fun a b -> [%expr [%e a]; [%e b]]]. *)
val seq_reduce : ?sep:expression -> expression -> expression -> expression

(** [binop_reduce] ≡ [fun x a b -> [%expr [%e x] [%e a] [%e b]]]. *)
val binop_reduce : expression -> expression -> expression -> expression

(** [strong_type_of_type ty] transform a type ty to
    [freevars . ty], giving a strong polymorphic type *)
val strong_type_of_type: core_type -> core_type

(** The mapper for the currently loaded deriving plugins. It is useful for
    recursively processing expression-valued attributes. *)
val mapper : Ast_mapper.mapper

(** {2 Miscellanea} *)

(** [hash_variant x] ≡ [Btype.hash_variant x]. *)
val hash_variant : string -> int
