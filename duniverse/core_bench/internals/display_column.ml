type t =
  [ `Name
  | `Speedup
  | `Percentage
  | `Samples
  ]
[@@deriving compare, sexp]

let equal = [%compare.equal: t]
