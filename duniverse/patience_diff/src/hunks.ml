module Stable = struct
  open! Core.Core_stable
  module Hunk = Hunk.Stable

  module V1 = struct
    type 'a t = 'a Hunk.V1.t list [@@deriving sexp, bin_io]
  end
end

open! Core
include Stable.V1

let concat_map_ranges hunks ~f = List.map hunks ~f:(Hunk.concat_map ~f)

let unified hunks =
  let f : 'a Range.t -> 'a Range.t list = function
    | Replace (l_range, r_range) -> [ Prev l_range; Next r_range ]
    | range -> [ range ]
  in
  concat_map_ranges hunks ~f
;;

let ranges hunks = List.concat_map hunks ~f:(fun hunk -> hunk.Hunk.ranges)
