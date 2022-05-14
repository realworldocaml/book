open! Base
open! Ppxlib

module Generic : sig
  type 'specific t =
    | Omit_nil
    | Sexp_array of core_type
    | Sexp_bool
    | Sexp_list of core_type
    | Sexp_option of core_type
    | Specific of 'specific
end

module Of_sexp : sig
  type t =
    | Default of expression Lifted.t
    | Required

  val create : loc:Location.t -> label_declaration -> t Generic.t
end

module Sexp_of : sig
  module Drop : sig
    type t =
      | No_arg
      | Compare
      | Equal
      | Sexp
      | Func of expression Lifted.t
  end

  type t =
    | Drop_default of Drop.t
    | Drop_if of expression Lifted.t
    | Keep

  val create : loc:Location.t -> label_declaration -> t Generic.t
end

(** Lift the contents of [Attrs.default]. *)
val lift_default : loc:location -> label_declaration -> expression -> expression Lifted.t
