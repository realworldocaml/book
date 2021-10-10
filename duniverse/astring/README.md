Astring — Alternative String module for OCaml
-------------------------------------------------------------------------------
%%VERSION%%

Astring exposes an alternative `String` module for OCaml. This module
tries to balance minimality and expressiveness for basic, index-free,
string processing and provides types and functions for substrings,
string sets and string maps.

Remaining compatible with the OCaml `String` module is a non-goal. The
`String` module exposed by Astring has exception safe functions,
removes deprecated and rarely used functions, alters some signatures
and names, adds a few missing functions and fully exploits OCaml's
newfound string immutability.

Astring depends only on the OCaml standard library. It is distributed
under the ISC license.

Home page: http://erratique.ch/software/astring  

## Installation

Astring can be installed with `opam`:

    opam install astring

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is automatically generated by
`ocamldoc` from the interfaces. It can be consulted [online][doc]
or via `odig doc astring`.

[doc]: http://erratique.ch/software/astring/doc/

## Sample programs

If you installed Astring with `opam` sample programs are located in
the directory `opam config var astring:doc`.