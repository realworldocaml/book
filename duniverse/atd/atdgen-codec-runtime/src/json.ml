
type t =
  [ `Assoc of (string * t) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `Intlit of string
  | `List of t list
  | `Null
  | `String of string
  | `Tuple of t list
  | `Variant of string * t option ]

let constr0 s = `Variant (s, None)

let constr1 s a = `Variant (s, Some a)
