open Sexplib.Std

type t = {
  foo: int;
  bar: string
} [@@deriving sexp, fields]
