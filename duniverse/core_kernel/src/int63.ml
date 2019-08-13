open! Import

module Bin : Binable0.S with type t := Base.Int63.t = struct
  module Bin_emulated = struct
    type t = Base.Not_exposed_properly.Int63_emul.t

    include Binable0.Of_binable
        (Int64)
        (struct
          type nonrec t = t

          let of_binable = Base.Not_exposed_properly.Int63_emul.W.wrap_exn
          let to_binable = Base.Not_exposed_properly.Int63_emul.W.unwrap
        end)
  end

  type 'a binable = (module Binable0.S with type t = 'a)

  let binable_of_repr : type a b. (a, b) Base.Int63.Private.Repr.t -> b binable =
    function
    | Base.Int63.Private.Repr.Int -> (module Int)
    | Base.Int63.Private.Repr.Int64 -> (module Bin_emulated)
  ;;

  let binable : Base.Int63.t binable = binable_of_repr Base.Int63.Private.repr

  include (val binable)

  let bin_shape_t = Bin_prot.Shape.bin_shape_int63
end

module Stable = struct
  module V1 = struct
    module T = struct
      type t = Base.Int63.t [@@deriving hash, sexp]

      include Bin

      include (
        Base.Int63 :
          Base.Comparable.S
        with type t := t
        with type comparator_witness = Base.Int63.comparator_witness)
    end

    include T
    include Comparable.Stable.V1.Make (T)
  end
end

module type Typerepable = sig
  type t [@@deriving typerep]
end

type 'a typerepable = (module Typerepable with type t = 'a)

let typerep_of_repr : type a b. (a, b) Base.Int63.Private.Repr.t -> a typerepable =
  function
  | Base.Int63.Private.Repr.Int -> (module Int)
  | Base.Int63.Private.Repr.Int64 -> (module Int64)
;;

let typerepable : Base.Int63.t typerepable = typerep_of_repr Base.Int63.Private.repr

include (val typerepable)

module Replace_polymorphic_compare : Comparable.Polymorphic_compare with type t := t =
  Base.Int63

module I =
  Identifiable.Extend
    (Base.Int63)
    (struct
      type nonrec t = t

      include Bin
    end)

include (
  I :
    module type of struct
    include I
  end
  with module Replace_polymorphic_compare := I.Replace_polymorphic_compare)

module Hex = struct
  type nonrec t = t [@@deriving typerep, bin_io]

  include (
    Base.Int63.Hex :
      module type of struct
      include Base.Int63.Hex
    end
    with type t := t)
end

include (
  Base.Int63 :
    module type of struct
    include Base.Int63
  end
  with type t := t
  with module Hex := Base.Int63.Hex)

let quickcheck_generator = Base_quickcheck.Generator.int63
let quickcheck_observer = Base_quickcheck.Observer.int63
let quickcheck_shrinker = Base_quickcheck.Shrinker.int63
let gen_incl = Base_quickcheck.Generator.int63_inclusive
let gen_uniform_incl = Base_quickcheck.Generator.int63_uniform_inclusive
let gen_log_incl = Base_quickcheck.Generator.int63_log_inclusive
let gen_log_uniform_incl = Base_quickcheck.Generator.int63_log_uniform_inclusive
