open! Import

type 'a t = 'a option [@@deriving bin_io, typerep]

include (
  Base.Option :
    module type of struct
    include Base.Option
  end
  with type 'a t := 'a t)

include Comparator.Derived (struct
    type nonrec 'a t = 'a t [@@deriving sexp_of, compare]
  end)

let quickcheck_generator = Base_quickcheck.Generator.option
let quickcheck_observer = Base_quickcheck.Observer.option
let quickcheck_shrinker = Base_quickcheck.Shrinker.option

module Stable = struct
  module V1 = struct
    type nonrec 'a t = 'a t [@@deriving bin_io, compare, sexp]
  end
end
