Mdx used to fail with module aliases and includes. This is a regression test to
ensure nothing breaks again in the future.

```ocaml
# module Test = String;;
module Test = String
```

```ocaml
# include struct module M = struct type t end end;;
module M : sig type t end
```

The bug was caused by an attempt to load the newly created module without
checking if it was supposed to be in the store (advertised by the
`module_presence` type), or without using the correct name.
