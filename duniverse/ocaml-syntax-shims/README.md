ocaml-syntax-shims
==================

This package provides a small utility that backports some of the newer
OCaml syntax to older OCaml compilers. This allows adopting new
features such as `let+` while still keeping compatibility with older
OCaml compiler.

To use it, simply depend on this package and add the following field
to your `library` or `executable` stanzas in your `dune` files:
`(preprocess future_syntax)`. For instance:

```scheme
(library
 (name mylib)
 (preprocess future_syntax))
```
