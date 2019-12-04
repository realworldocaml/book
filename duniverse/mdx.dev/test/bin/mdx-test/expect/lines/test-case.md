Testing lines directives

```ocaml
let f x =
  x+x
  +x
```

And

```ocaml version=4.02
# let f x = x + 1
val f : int -> int = <fun>
# let f y =
  y^"foo"
val f : bytes -> bytes = <fun>
```

```ocaml version>=4.06
# let f x = x + 1
val f : int -> int = <fun>
# let f y =
  y^"foo"
val f : string -> string = <fun>
```

And

```ocaml version=4.02
# let f x = function
  | 0 -> 1
  | n ->
  n + "foo"
Characters 45-50:
Error: This expression has type bytes but an expression was expected of type
         int
```

```ocaml version=4.06
# let f x = function
  | 0 -> 1
  | n ->
  n + "foo"
Characters 45-50:
Error: This expression has type string but an expression was expected of type
         int
```

```ocaml version=4.07
# let f x = function
  | 0 -> 1
  | n ->
  n + "foo"
Characters 45-50:
Error: This expression has type string but an expression was expected of type
         int
```

```ocaml version>=4.08
# let f x = function
  | 0 -> 1
  | n ->
  n + "foo"
Line 4, characters 7-12:
Error: This expression has type string but an expression was expected of type
         int
```

Line directives work for `sh` blocks as well:

```sh version=4.08.1
$ ocamlc -version
4.08.1
```

```sh version=4.07.1
$ ocamlc -version
4.07.1
```
