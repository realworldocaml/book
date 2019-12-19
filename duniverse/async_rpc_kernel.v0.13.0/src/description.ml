open Core_kernel

module Stable = struct
  module V1 = struct
    type t =
      { name    : string
      ; version : int
      }
    [@@deriving bin_io, compare, hash, sexp]
  end
end

include Stable.V1
include Comparable.Make (Stable.V1)
include Hashable.Make (Stable.V1)
