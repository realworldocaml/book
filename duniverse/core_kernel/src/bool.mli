(** This module extends {{!Base.Bool}[Base.Bool]}. *)

type t = bool [@@deriving bin_io, typerep]

include module type of Base.Bool with type t := t

include
  Identifiable.S
  with type t := t
   and type comparator_witness := Base.Bool.comparator_witness

include Quickcheckable.S with type t := t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving compare, sexp, bin_io]
  end
end
