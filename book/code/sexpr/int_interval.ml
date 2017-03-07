(* Module for representing closed integer intervals *)
open Core

(* Invariant: For any Range (x,y), y >= x *)
type t =
  | Range of int * int
  | Empty
[@@deriving sexp]

let is_empty =
  function
  | Empty -> true
  | Range _ -> false

let create x y =
  if x > y then
    Empty
  else
    Range (x,y)

let contains i x =
  match i with
  | Empty -> false
  | Range (low,high) -> x >= low && x <= high
