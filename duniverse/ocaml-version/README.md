ocaml-version -- Manipulate, parse and generate OCaml compiler version strings
------------------------------------------------------------------------------

This library provides facilities to parse version numbers of the OCaml
compiler, and enumerates the various official OCaml releases and configuration
variants.

OCaml version numbers are of the form `major.minor.patch+extra`, where the
`patch` and `extra` fields are optional.  This library offers the following
functionality:

- Functions to parse and serialise OCaml compiler version numbers.
- Enumeration of official OCaml compiler version releases.
- Test compiler versions for a particular feature (e.g. the `bytes` type)
- [opam](https://opam.ocaml.org) compiler switch enumeration.

Browse the [API documentation](https://ocurrent.github.io/ocaml-version/doc) for more
details.

### Further information

- **Discussion:** Post on <https://discuss.ocaml.org/> with the `ocaml` tag under
  the Ecosystem category.
- **Bugs:** <https://github.com/ocurrent/ocaml-version/issues>
- **Docs:** <http://docs.mirage.io/ocaml-version>

Contributions are very welcome.  Please see the overall TODO list below, or
please get in touch with any particular comments you might have.

[![Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Focurrent%2Focaml-version%2Fmaster&logo=ocaml)](https://ci.ocamllabs.io/github/ocurrent/ocaml-version)

### TODO 

- Complete the architecture set from the officially supported compilers.
- Add more features to the opam variants list (such as `Since.safe_string`)
- Generate the core opam compiler package set purely from this library, so that
  it remains in sync with the library description.
