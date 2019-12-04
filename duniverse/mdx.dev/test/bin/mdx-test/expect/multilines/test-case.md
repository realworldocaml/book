Test multi-lines shell commands:

```sh
$ for i in `seq 1 10`; do \
>   echo $i; \
> done
1
...
10
```

This works trivially for normal OCaml fragments:

```ocaml
let rec fact = function
| 1 -> 1
| n -> n * fact (n-1)
```

And it should work fine for toplevel too:

```ocaml
# let rec fact = function
  | 1 -> 1
  | n -> n * fact (n-1)
  ;;
val fact : int -> int = <fun>
```

```ocaml
# print_string "foo \
  \" \
  toto\
  \ bar\""
foo " toto bar"
- : unit = ()
```
