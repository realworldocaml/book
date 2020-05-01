(** This module extends {{!Base.Option}[Base.Option]} with bin_io and quickcheck. *)

type 'a t = 'a Base.Option.t [@@deriving bin_io, typerep]

(** @open *)
include module type of struct
  include Base.Option
end
with type 'a t := 'a option

include Comparator.Derived with type 'a t := 'a t
include Quickcheckable.S1 with type 'a t := 'a t

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t [@@deriving bin_io, compare, sexp]
  end
end
