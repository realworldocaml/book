# stdlib-shims

Compiling against this library allows to replace your uses of `Pervasives` with `Stdlib` before `4.08`. For example you can use `Stdlib.compare` instead of `Pervasives.compare`. It does not however provide the new functions and modules that are being added in `Stdlib` module.

## Installation

To install the project via [opam](https://opam.ocaml.org), the
standard package manager for OCaml, type:

```
opam install stdlib-shims
```
