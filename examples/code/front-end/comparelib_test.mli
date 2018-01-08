open Core_kernel

type t = {
  foo: string;
  bar: t
} [@@deriving compare]
