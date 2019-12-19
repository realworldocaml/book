open! Import

type t = bool [@@deriving bin_io, typerep]

include Identifiable.Extend
    (Base.Bool)
    (struct
      type nonrec t = t [@@deriving bin_io]
    end)

module Replace_polymorphic_compare = Base.Bool

include (
  Base.Bool :
    module type of struct
    include Base.Bool
  end
  with type t := t)

let quickcheck_generator = Base_quickcheck.Generator.bool
let quickcheck_observer = Base_quickcheck.Observer.bool
let quickcheck_shrinker = Base_quickcheck.Shrinker.bool

module Stable = struct
  module V1 = struct
    type nonrec t = t [@@deriving compare, sexp, bin_io]
  end
end
