# ppx_deriving_accessors

This folder contains an example of a very simple ppx deriver that will generate
accessors for record fields from the record type definition.

E.g. the following:

```ocaml
type t =
  { a : string
  ; b : int
  }
  [@@deriving accessors]
```

will generate the following:

```ocaml
let a x = x.a
let b x = x.b
```

It can also be used in `.mli` files to generate the corresponding signatures:

```ocaml
val a : t -> string
val b : t -> int
```
