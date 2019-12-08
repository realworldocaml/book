open Ppx_compare_lib.Builtin
open Ppx_sexp_conv_lib.Conv

type t =
  | Regexp  of string
  | Glob    of string
  | Literal of string
[@@deriving sexp_of, variants, compare]
