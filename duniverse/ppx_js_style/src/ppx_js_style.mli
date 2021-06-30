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

module Suspicious_literal : sig
  type t
end

module Invalid_ocamlformat_attribute : sig
  type t
end

type error =
  | Invalid_deprecated of Invalid_deprecated.t
  | Missing_type_annotation of Ignored_reason.t
  | Invalid_constant of Invalid_constant.t
  | Suspicious_literal of Suspicious_literal.t
  | Invalid_ocamlformat_attribute of Invalid_ocamlformat_attribute.t
  | Docstring_on_open
  | Use_of_letop of { op_name : string }

val iter_style_errors :
  f:(loc:Location.t -> error -> unit) -> Ast_traverse.iter

val check : Ast_traverse.iter

(** If [true] (the default) complain when using [@inline never] instead of [@cold] *)
val cold_instead_of_inline_never : bool ref
