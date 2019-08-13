type t = Init | Set_by_inline_test
[@@deriving sexp, compare]

val value : unit -> t
