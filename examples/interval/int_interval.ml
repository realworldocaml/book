(* file: int_interval.ml *)
open Core.Std

type t = | Range of int * int
         | Empty
with sexp

let empty = Empty
let create x y = if x > y then Empty else Range (x,y)
let contains i x = match i with
   | Empty -> false
   | Range (low,high) -> x >= low && x <= high
