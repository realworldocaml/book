open Sexplib.Std

type line = [
  | `Output  of string
  | `Command of string
  | `Comment of string
  | `Part    of string
  | `Non_det of [`Command|`Output]
] [@@deriving sexp]
