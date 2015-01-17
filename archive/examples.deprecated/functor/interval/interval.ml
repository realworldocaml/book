(* interval.ml *)

open Core.Std
open Int.Replace_polymorphic_compare

module type Comparable = sig
  type t
  val compare : t -> t -> int
end

module Make(C : Comparable) = struct
  type t = | Interval of C.t * C.t
           | Empty

  let create low high =
    if C.compare low high > 0 then Empty
    else Interval (low,high)

  let contains t x =
    match t with
    | Empty -> false
    | Interval (l,h) ->
      C.compare x l >= 0 && C.compare x h <= 0

  let intersect t1 t2 =
    let min x y = if C.compare x y <= 0 then x else y in
    let max x y = if C.compare x y >= 0 then x else y in
    match t1,t2 with
    | Empty, _ | _, Empty -> Empty
    | Interval (l1,h1), Interval (l2,h2) ->
      create (max l1 l2) (min h1 h2)

end


