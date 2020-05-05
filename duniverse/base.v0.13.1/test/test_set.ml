open! Import
open! Set

type int_set = Set.M(Int).t [@@deriving compare, equal, hash, sexp]

let%test _ = invariants (of_increasing_iterator_unchecked (module Int) ~len:20 ~f:Fn.id)
let%test _ = invariants (Poly.of_increasing_iterator_unchecked ~len:20 ~f:Fn.id)

module Poly = struct
  let%test _ = length Poly.empty = 0
  let%test _ = Poly.equal (Poly.of_list []) Poly.empty

  let%test _ =
    let a = Poly.of_list [ 1; 1 ] in
    let b = Poly.of_list [ "a" ] in
    length a = length b
  ;;
end
