Different environments can have different preludes

This is run with `--prelude all.ml --prelude " :default.ml" --prelude a:a.ml --prelude b:b.ml`.
Each of them defines a variable of the same name.

```ocaml
# all;;
- : int = 1
# default;;
- : int = 1
# a;;
...
Error: Unbound value a
# b;;
...
Error: Unbound value b
```

```ocaml env=a
# all;;
- : int = 1
# default;;
...
Error: Unbound value default
# a;;
- : int = 1
# b;;
...
Error: Unbound value b
```

```ocaml env=b
# all;;
- : int = 1
# default;;
...
Error: Unbound value default
# a;;
...
Error: Unbound value a
# b;;
- : int = 1
```

```ocaml env=c
# all;;
- : int = 1
# default;;
...
Error: Unbound value default
# a;;
...
Error: Unbound value a
# b;;
...
Error: Unbound value b
```
