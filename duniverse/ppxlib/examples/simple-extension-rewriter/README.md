# ppx_get_env

This folder contains an example of a very simple ppx rewriter that will expand
`[%get_env "SOME_ENV_VAR"]` into the value of the env variable `SOME_ENV_VAR` at compile time,
as a string.

E.g., assuming we set `MY_VAR="foo"`, it will turn:

```ocaml
let () = print_string [%get_env "foo"]
```

into:

```ocaml
let () = print_string "foo"
```

Note that this is just a toy example and we'd actually advise you against this type of ppx
that have side effects or rely heavily on the file system or env variables unless you absolutely
what your doing.

In particular in this case it won't work well with dune since dune won't know about the dependency
on the env variables specified in the extension's payload.
