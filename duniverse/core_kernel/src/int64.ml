open! Import

type t = int64 [@@deriving typerep]

include Identifiable.Extend
    (Base.Int64)
    (struct
      type t = int64 [@@deriving bin_io]
    end)

module Hex = struct
  type nonrec t = t [@@deriving typerep, bin_io]

  include (
    Base.Int64.Hex :
      module type of struct
      include Base.Int64.Hex
    end
    with type t := t)
end

include (
  Base.Int64 :
    module type of struct
    include Base.Int64
  end
  with type t := t
  with module Hex := Base.Int64.Hex)

let quickcheck_generator = Base_quickcheck.Generator.int64
let quickcheck_observer = Base_quickcheck.Observer.int64
let quickcheck_shrinker = Base_quickcheck.Shrinker.int64
let gen_incl = Base_quickcheck.Generator.int64_inclusive
let gen_uniform_incl = Base_quickcheck.Generator.int64_uniform_inclusive
let gen_log_incl = Base_quickcheck.Generator.int64_log_inclusive
let gen_log_uniform_incl = Base_quickcheck.Generator.int64_log_uniform_inclusive
