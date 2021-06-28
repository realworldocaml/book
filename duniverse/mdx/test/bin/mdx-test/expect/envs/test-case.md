We can define separate environments for blocks.

`x` holds the value `1` in the environment `e1`.

```ocaml env=e1
let x = 1;;
```

```ocaml env=e1
module M = struct let k = 42 let f x = x * k end;;
```

`x` holds the value `3` in the environment `e2`.

```ocaml env=e2
let x = 3;;
```

We can retrieve the value of `x` in environment `e1`:

```ocaml env=e1
# print_int x;;
1
- : unit = ()
# print_int M.k;;
42
- : unit = ()
# M.f;;
- : int -> int = <fun>
```

More tests:
```ocaml
# let x = 42;;
val x : int = 42
# type t1 = int;;
type t1 = int
```

```ocaml env=e1
# let x = 1;;
val x : int = 1
# type t1 = int;;
type t1 = int
```

```ocaml
# print_int x;;
42
- : unit = ()
# type t2 = t1;;
type t2 = t1
```

```ocaml env=e1
# print_int x;;
1
- : unit = ()
# type t2 = t1;;
type t2 = t1
```

```ocaml version<4.08,env=e2
# let x = 2;;
val x : int = 2
# type t2 = int;;
type t2 = int
# type t3 = t1;;
Characters 10-12:
Error: Unbound type constructor t1
```

```ocaml version>=4.08,env=e2
# let x = 2;;
val x : int = 2
# type t2 = int;;
type t2 = int
# type t3 = t1;;
Line 1, characters 11-13:
Error: Unbound type constructor t1
```

```ocaml
# type t2 = t1;;
type t2 = t1
# let (y : t1) = 4;;
val y : t2 = 4
```

```ocaml
# type t3 = t2;;
type t3 = t2
# print_int y;;
4
- : unit = ()
# let (x : t2) = 1;;
val x : t3 = 1
# print_int x;;
1
- : unit = ()
# let (y : t3) = 2;;
val y : t3 = 2
# print_int y;;
2
- : unit = ()
```

```ocaml version<4.08,env=e2
# let (z : t2) = 32;;
val z : t2 = 32
# print_int z;;
32
- : unit = ()
# let (d : t3) = 32;;
Characters 9-11:
Error: Unbound type constructor t3
# print_int d;;
Characters 10-11:
Error: Unbound value d
```

```ocaml version>=4.08,env=e2
# let (z : t3) = 32;;
Line 1, characters 10-12:
Error: Unbound type constructor t3
# print_int z;;
Line 1, characters 11-12:
Error: Unbound value z
# let (d : t4) = 32;;
Line 1, characters 10-12:
Error: Unbound type constructor t4
# print_int d;;
Line 1, characters 11-12:
Error: Unbound value d
```
