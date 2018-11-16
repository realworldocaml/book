open Sexplib.Std

type line = [
  | `Output  of string
  | `Command of string
  | `Comment of string
  | `Part    of string
  | `Ellipsis
  | `Exit_code of int
  | `Non_det of [`Command|`Output]
] [@@deriving sexp]
