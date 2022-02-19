(* The [Int] module has become standard in OCaml 4.08.0. *)

type t = int

let equal : int -> int -> bool =
  (=)

let compare : int -> int -> int =
  compare
