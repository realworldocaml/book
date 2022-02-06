(** Context free rewriting *)

open! Import

(** Local rewriting rules.

    This module lets you define local rewriting rules, such as extension point
    expanders. It is not completely generic and you cannot define any kind of
    rewriting, it currently focuses on what is commonly used. New scheme can be
    added on demand.

    We have some ideas to make this fully generic, but this hasn't been a
    priority so far. *)
module Rule : sig
  type t

  val extension : Extension.t -> t
  (** Rewrite an extension point *)

  val special_function : string -> (expression -> expression option) -> t
  (** [special_function id expand] is a rule to rewrite a function call at
      parsing time. [id] is the identifier to match on and [expand] is used to
      expand the full function application (it gets the Pexp_apply node). If the
      function is found in the tree without being applied, [expand] gets only
      the identifier (Pexp_ident node) so you should handle both cases.

      If [id] is an operator identifier and contains dots, it should be
      parenthesized (e.g. ["(+.+)"]).

      [expand] must decide whether the expression it receive can be rewritten or
      not. Especially ppxlib makes the assumption that [expand] is idempotent.
      It will loop if it is not. *)

  (** Used for the [constant] function. *)
  module Constant_kind : sig
    type t = Float | Integer
  end

  val constant :
    Constant_kind.t ->
    char ->
    (Location.t -> string -> Parsetree.expression) ->
    t
  (** [constant kind suffix expander] Registers an extension for transforming
      constants literals, based on the suffix character. *)

  (** The rest of this API is for rewriting rules that apply when a certain
      attribute is present. The API is not complete and is currently only enough
      to implement deriving. *)

  type ('a, 'b, 'c) attr_group_inline =
    ('b, 'c) Attribute.t ->
    (ctxt:Expansion_context.Deriver.t ->
    Asttypes.rec_flag ->
    'b list ->
    'c option list ->
    'a list) ->
    t
  (** Match the attribute on a group of items, such as a group of recursive type
      definitions (Pstr_type, Psig_type). The expander will be triggered if any
      of the item has the attribute. The expander is called as follow:

      [expand ~loc ~path rec_flag items values]

      where [values] is the list of values associated to the attribute for each
      item in [items]. [expand] must return a list of element to add after the
      group. For instance a list of structure item to add after a group of type
      definitions. *)

  val attr_str_type_decl :
    (structure_item, type_declaration, _) attr_group_inline

  val attr_sig_type_decl :
    (signature_item, type_declaration, _) attr_group_inline

  val attr_str_type_decl_expect :
    (structure_item, type_declaration, _) attr_group_inline
  (** The _expect variants are for producing code that is compared to what the
      user wrote in the source code. *)

  val attr_sig_type_decl_expect :
    (signature_item, type_declaration, _) attr_group_inline

  type ('a, 'b, 'c) attr_inline =
    ('b, 'c) Attribute.t ->
    (ctxt:Expansion_context.Deriver.t -> 'b -> 'c -> 'a list) ->
    t
  (** Same as [attr_group_inline] but for elements that are not part of a group,
      such as exceptions and type extensions *)

  val attr_str_module_type_decl :
    (structure_item, module_type_declaration, _) attr_inline

  val attr_sig_module_type_decl :
    (signature_item, module_type_declaration, _) attr_inline

  val attr_str_module_type_decl_expect :
    (structure_item, module_type_declaration, _) attr_inline

  val attr_sig_module_type_decl_expect :
    (signature_item, module_type_declaration, _) attr_inline

  val attr_str_type_ext : (structure_item, type_extension, _) attr_inline

  val attr_sig_type_ext : (signature_item, type_extension, _) attr_inline

  val attr_str_type_ext_expect : (structure_item, type_extension, _) attr_inline

  val attr_sig_type_ext_expect : (signature_item, type_extension, _) attr_inline

  val attr_str_exception : (structure_item, type_exception, _) attr_inline

  val attr_sig_exception : (signature_item, type_exception, _) attr_inline

  val attr_str_exception_expect :
    (structure_item, type_exception, _) attr_inline

  val attr_sig_exception_expect :
    (signature_item, type_exception, _) attr_inline
end

(**/**)

(*_ This API is not stable *)
module Generated_code_hook : sig
  type 'a single_or_many = Single of 'a | Many of 'a list

  (*_ Hook called whenever we generate code some *)
  type t = {
    f : 'a. 'a Extension.Context.t -> Location.t -> 'a single_or_many -> unit;
  }

  val nop : t
end

module Expect_mismatch_handler : sig
  type t = {
    f : 'a. 'a Attribute.Floating.Context.t -> Location.t -> 'a list -> unit;
  }

  val nop : t
end

(**/**)

(* TODO: a simple comment here is fine, while we would expect only docstring or (*_ *)
   comments to be accepted. On the contrary, docstrings are *not* accepted.

   This means https://github.com/ocaml/ocaml/pull/477 was not complete and indeed the
   parser should be fixed. *)
class map_top_down :
  ?expect_mismatch_handler:
    Expect_mismatch_handler.t (* default: Expect_mismatch_handler.nop *)
  -> ?generated_code_hook:
       Generated_code_hook.t (* default: Generated_code_hook.nop *)
  -> Rule.t list
  -> Ast_traverse.map_with_expansion_context
