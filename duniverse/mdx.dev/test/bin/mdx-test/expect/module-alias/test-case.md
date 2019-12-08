Mdx used to fail with module aliases, this is a regression test to ensure nothing breaks again in the future.

```ocaml
# module Test = String;;
module Test = String
```

The bug was caused by an attempt to load the newly created module without checking if it was supposed to be in the store (advertised by the module_presence type).
