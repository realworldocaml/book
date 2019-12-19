(** A module internal to [Core_bench]. Please look at {!Bench}. *)

type t =
  [ `Name
  | `Speedup
  | `Percentage
  | `Samples
  ]
[@@deriving compare, sexp]

let equal = [%compare.equal: t]
