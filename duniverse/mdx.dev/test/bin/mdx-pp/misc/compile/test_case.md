Mdx can be used as a preprocessor to compile this file as if it was just
comprised of the content of its ocaml blocks

```ocaml
let msg = "hello"
```

It should work as a preprocessor for dune using the `preprocess` field of `executable`
and `library` stanzas:

```
(library
 (name <lib_name>)
 (preprocess (action (run ocaml-mdx pp %{input-file}))))
```

although it currently require defining a dialect in your dune-project so that
dune recognizes `.md` files as implementation files:

```
(dialect
 (name md)
 (implementation (extension md))
 (interface (extension mdi)))
```

It should be possible to use the dialect alone (without the `preprocess` field
in `library` or `executable` stanzas) if we can get `ocamlc -pp "ocaml-mdx pp"`
to just dump the binary AST to stdout without further compiling the input file.

Compiling this file as an executable and running it should print `hello` on stdout!

```ocaml
let () = print_endline msg
```
