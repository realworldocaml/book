module Stable = struct
  open! Core.Core_stable
  module Range = Range.Stable

  module V1 = struct
    type 'a t =
      { prev_start : int
      ; prev_size : int
      ; next_start : int
      ; next_size : int
      ; ranges : 'a Range.V1.t list
      }
    [@@deriving fields, sexp, bin_io]
  end
end

open! Core
include Stable.V1

let _invariant t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
    [%test_result: int]
      (List.sum (module Int) t.ranges ~f:Range.prev_size)
      ~expect:t.prev_size
      ~message:"prev_size";
    [%test_result: int]
      (List.sum (module Int) t.ranges ~f:Range.next_size)
      ~expect:t.next_size
      ~message:"next_size")
;;

let all_same hunk = Range.all_same hunk.ranges
let concat_map t ~f = { t with ranges = List.concat_map t.ranges ~f }
