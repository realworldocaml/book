Mdx pp should properly locate lines in generated code, even when the original
code block had empty lines

```ocaml


let x =

  5

```

```ocaml

# let x = 1

  in x
- : int = 1


# 3
- : int = 3


# Printf.printf "foo\n\nbar\n"
foo

bar
- : unit = ()
```
