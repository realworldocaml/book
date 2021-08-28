open Core

type t = {
  foo: string;
  bar: t
} [@@deriving compare]
