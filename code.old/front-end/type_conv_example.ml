open Sexplib.Std

type t = {
  foo: int;
  bar: string
} with sexp, fields
