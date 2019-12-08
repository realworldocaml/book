Mdx can also understand ocaml code blocks:


```ocaml file=test_case.ml,part=toto
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

```ocaml file=test_case.ml,part=zzz
let f = "hello world!"
```

```ocaml file=test_case.ml
x;;
```

```ocaml
# let x = 2;;
val x : int = 2
# print_int x;;
3
- : unit = ()
```

```ocaml file=test_case.ml,part=new-part-not-in-original
let g = [ f ]
```

```ocaml file=broken.ml
let () =
  module MyString = String;
  ()
```

```ocaml file=test_case.mli,part=1
module A =
struct
```

```ocaml file=test_case.mli,part=2
  type t = Some of int | Many
```

```ocaml file=test_case.mli,part=3
end
```
