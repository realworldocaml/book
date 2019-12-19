open! Import

module Stable = struct
  module V1 = struct
    include Base.Bytes

    type t = bytes [@@deriving bin_io, typerep]
  end
end

include Stable.V1

include Hexdump.Of_indexable (struct
    type t = bytes

    let length = length
    let get = get
  end)

let quickcheck_generator =
  String.quickcheck_generator |> Quickcheck.Generator.map ~f:of_string
;;

let quickcheck_observer =
  String.quickcheck_observer |> Quickcheck.Observer.unmap ~f:to_string
;;

let quickcheck_shrinker =
  String.quickcheck_shrinker |> Quickcheck.Shrinker.map ~f:of_string ~f_inverse:to_string
;;

let gen' char_gen = String.gen' char_gen |> Quickcheck.Generator.map ~f:of_string

let gen_with_length len char_gen =
  String.gen_with_length len char_gen |> Quickcheck.Generator.map ~f:of_string
;;
