`ocaml-mdx test` can also take a prelude file.

```ocaml
# Lwt.return 4
- : int = 4
```

It's possible to load a prelude only in a specific environment using
`--prelude=toto:prelude.ml`:

```ocaml env=toto
# print_endline x
42
- : unit = ()
```

```ocaml version<4.08
# print_endline x
Characters 14-15:
Error: Unbound value x
```

```ocaml version>=4.08
# print_endline x
Line 1, characters 15-16:
Error: Unbound value x
```
