# Bigstringaf

The OCaml compiler has a bunch of intrinsics for Bigstrings, but they're not
widely-known, sometimes misused, and programs that use Bigstrings are slower
than they have to be.  And even if a library got that part right and exposed
the intrinsics properly, the compiler doesn't have any fast blits between
Bigstrings and other string-like types.

So here they are. Go crazy.

[![Build Status](https://github.com/inhabitedtype/bigstringaf/workflows/build/badge.svg)](https://github.com/inhabitedtype/bigstringaf/actions?query=workflow%3A%22build%22)

## Installation

Install the library and its dependencies via [OPAM][opam]:

[opam]: http://opam.ocaml.org/

```bash
opam install bigstringaf
```

## Development

To install development dependencies, pin the package from the root of the
repository:

```bash
opam pin add -n bigstringaf .
opam install --deps-only bigstringaf
```

After this, you may install a development version of the library using the
install command as usual.

For building and running the tests during development, you will need to install
the `alcotest` package:

```bash
opam install alcotest
make test
```

## License

BSD3, see LICENSE file for its text.
