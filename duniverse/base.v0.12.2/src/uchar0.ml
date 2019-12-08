open! Import0

type t = Caml.Uchar.t

let succ           = Caml.Uchar.succ
let pred           = Caml.Uchar.pred
let is_valid       = Caml.Uchar.is_valid
let is_char        = Caml.Uchar.is_char
let unsafe_to_char = Caml.Uchar.unsafe_to_char
let unsafe_of_int  = Caml.Uchar.unsafe_of_int

let of_int = Caml.Uchar.of_int
let to_int = Caml.Uchar.to_int

let of_char = Caml.Uchar.of_char

let compare   = Caml.Uchar.compare
let equal     = Caml.Uchar.equal

let min_value = Caml.Uchar.min
let max_value = Caml.Uchar.max
