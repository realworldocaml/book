module Stable = struct
  open! Core.Core_stable

  module V1 = struct
    type 'a t =
      | Same of ('a * 'a) array
      | Prev of 'a array
      | Next of 'a array
      | Replace of 'a array * 'a array
      | Unified of 'a array
    [@@deriving sexp, bin_io]
  end
end

open! Core
include Stable.V1

let all_same ranges =
  List.for_all ranges ~f:(fun range ->
    match range with
    | Same _ -> true
    | _ -> false)
;;

let prev_only ranges =
  let f = function
    | Replace (l_range, _) -> [ Prev l_range ]
    | Next _ -> []
    | range -> [ range ]
  in
  List.concat_map ranges ~f
;;

let next_only ranges =
  let f = function
    | Replace (_, r_range) -> [ Next r_range ]
    | Prev _ -> []
    | range -> [ range ]
  in
  List.concat_map ranges ~f
;;

let prev_size = function
  | Unified lines | Replace (lines, _) | Prev lines -> Array.length lines
  | Same lines -> Array.length lines
  | Next _ -> 0
;;

let next_size = function
  | Unified lines | Replace (_, lines) | Next lines -> Array.length lines
  | Same lines -> Array.length lines
  | Prev _ -> 0
;;
