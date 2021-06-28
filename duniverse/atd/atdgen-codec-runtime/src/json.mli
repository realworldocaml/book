
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

val constr0 : string -> t
val constr1 : string -> t -> t
