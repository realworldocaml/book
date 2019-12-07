open! Import

type t = int32 [@@deriving typerep]

include Identifiable.Extend
    (Base.Int32)
    (struct
      type t = int32 [@@deriving bin_io]
    end)

module Hex = struct
  type nonrec t = t [@@deriving typerep, bin_io]

  include (
    Base.Int32.Hex :
      module type of struct
      include Base.Int32.Hex
    end
    with type t := t)
end

include (
  Base.Int32 :
    module type of struct
    include Base.Int32
  end
  with type t := t
  with module Hex := Base.Int32.Hex)

let quickcheck_generator = Base_quickcheck.Generator.int32
let quickcheck_observer = Base_quickcheck.Observer.int32
let quickcheck_shrinker = Base_quickcheck.Shrinker.int32
let gen_incl = Base_quickcheck.Generator.int32_inclusive
let gen_uniform_incl = Base_quickcheck.Generator.int32_uniform_inclusive
let gen_log_incl = Base_quickcheck.Generator.int32_log_inclusive
let gen_log_uniform_incl = Base_quickcheck.Generator.int32_log_uniform_inclusive
