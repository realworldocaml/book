Yojson: low-level JSON library for OCaml
========================================

[![Build Status](https://travis-ci.org/ocaml-community/yojson.svg?branch=master)](https://travis-ci.org/ocaml-community/yojson)

_This library is for manipulating the json AST directly. For mapping between OCaml types and json, we recommend [atdgen](https://github.com/mjambon/atd)._

Library documentation
---------------------

Currently at https://ocaml-community.github.io/yojson/

Design goals
------------

* reducing inter-package dependencies by the use of polymorphic
  variants for the JSON tree type

* allowing variants of the JSON tree type to be shipped by the library
  itself or to be easily created as extensions of the library

* allowing type-aware serializers/deserializers such as json-static
  to read and write directly without going through a JSON tree,
  for efficiency purposes.
  This requires making readers and writers of JSON atoms (int, string,
  etc.) to be exported and composable.

* providing a few non-standard, optional extensions of JSON.
  These extensions will include:
  * optional quotes around "simple" field/constructor names
  * a syntax for tuples (at least 2 elements): `(x, y)`
  * a syntax for variants (0 or 1 arg only): `<Foo> <Bar:"abc">`


Other choices already in json-wheel
-----------------------------------

* distinction between ints and floats (optional)

* Getting rid of the UTF-X encoding constraint that prevents from
  exchanging binary data:
  * encoding is ASCII except for the contents of string literals
  * string literals may represent arbitrary sequence of bytes
  * `\uABCD` escapes in string literals expand to UTF-8


Miscellaneous
-------------

* no dependency on ocamlnet for UTF-8
