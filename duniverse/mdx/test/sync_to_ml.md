Mdx can also understand ocaml code blocks:


```ocaml file=sync_to_ml.ml,part=toto
# let x = 3;;
val x : int = 3
# let y = 4;;
val y : int = 4
# print_int x;;
3
- : unit = ()
# print_int y;;
4
- : unit = ()
```

```ocaml file=sync_to_ml.ml,part=zzz
let f = "hello world!"
```

```ocaml file=sync_to_ml.ml
x;;
```

```ocaml
# let x = 2;;
val x : int = 2
# print_int x;;
3
- : unit = ()
```

```ocaml file=sync_to_broken_ml.ml
let () =
  module MyString = String;
  ()
```

```ocaml file=sync_to_ml.mli,part=3
type t = Some of int | Many
```
