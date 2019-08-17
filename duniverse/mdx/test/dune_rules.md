Mdx can generate `dune` rules to synchronize .md files with .ml files.


```ocaml file=dune_rules_1.ml
# let x = 3;;
val x : int = 3
```

```ocaml file=dune_rules_2.ml
# let x = 4;;
val x : int = 4
```

```ocaml source-tree=foo
# let x = 2;;
val x : int = 2
```
