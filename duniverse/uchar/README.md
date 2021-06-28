Uchar — Compatibility library for OCaml's Uchar module
-------------------------------------------------------------------------------
Release %%VERSION%%

The `uchar` package provides a compatibility library for the
[`Uchar`][1] module introduced in OCaml 4.03.

The `uchar` package is distributed under the license of the OCaml
compiler. See [LICENSE](LICENSE) for details.

[1]: http://caml.inria.fr/pub/docs/manual-ocaml/libref/Uchar.html

Home page: https://ocaml.github.io/uchar/  
Contact: Daniel Bünzli `<daniel.buenzl i@erratique.ch>`


# Installation

Uchar can be installed with `opam`:

    opam install uchar

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.


# Documentation

Since OCaml 4.03 you can find the module in the documentation
of the [standard library][1]. Before this you can consult
[this page](https://ocaml.github.io/uchar/Uchar.html).


# Testing

The file `test/testpkg.ml` can be used to test the compilation of the
package. Once the package is installed the following invocation
should succeed both before and after 4.03

    ocamlbuild -X src -use-ocamlfind -pkg uchar test/testpkg.native
