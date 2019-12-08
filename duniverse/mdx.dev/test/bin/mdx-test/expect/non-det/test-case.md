`ocaml-mdx` supports non-determinitic code blocks.

There are two kinds of blocks:

### Non-deterministic Outputs

Code blocks with `non-deterministic=output` have their command always
executed but their output is never checked, unless `--non-deterministic`
is passed as argument to `ocaml-mdx`.


```sh non-deterministic=output
$ echo $RANDOM
4150
```

```ocaml non-deterministic=output
# Random.self_init (); Random.int 42
0
```

Check that the command are always executed:

```sh non-deterministic=output
$ touch hello-world
```

```ocaml
# Sys.file_exists "hello-world"
- : bool = true
```

### Non-deterministic Commands

Code blocks with `non-deterministic=command` are never executed unless
`--non-deterministic` is passed as argument to `ocaml-mdx`.

```sh non-deterministic=command
$ touch toto
```

```sh
$ touch bar
```


```ocaml
# Sys.file_exists "toto"
- : bool = false
# Sys.file_exists "bar"
- : bool = true
```
