open! Core_kernel

type t =
  [ `No
  | `Yes
  | `Require of string list
  |
    `Replace of string list
  | `Add of string list
  | `Transform of (string list -> string list) sexp_opaque
  | `Filter_map of (string list -> string option list) sexp_opaque ]
[@@deriving sexp_of]
