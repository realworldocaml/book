open Ppxlib

module Ignored_reason : sig
  type t = Argument_to_ignore | Underscore_pattern
end

module Invalid_deprecated : sig
  type t =
    | Not_a_string
    | Missing_date
    | Invalid_month
end

module Invalid_constant : sig
  type t
end

type error =
  | Invalid_deprecated of Invalid_deprecated.t
  | Missing_type_annotation of Ignored_reason.t
  | Invalid_constant of Invalid_constant.t
  | Docstring_on_open

val iter_style_errors :
  f:(loc:Location.t -> error -> unit) -> Ast_traverse.iter

val check : Ast_traverse.iter
