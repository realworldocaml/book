type s = { foo: int; bar: unit }
type t = { foo: int }

let f x =
  x.bar;
  x.foo
