***************
The ATD Project
***************

The ATD project aims to facilitate the design and the implementation of
APIs, and in particular JSON APIs. It offers type safety and automatic
data validation by deriving boilerplate code from type definitions.
The data ends up being represented with idiomatic data structures in
the target programming language, removing the hassle of manually
converting from/to the JSON representation.

Currently, the supported target languages are OCaml, Java, Scala, and
Python. The project is run by volunteers and users from various
organizations. Check out the
`ATD project on GitHub <https://github.com/ahrefs/atd>`_ for any bug
report, feature request, or question.

Some properties of interest of ATD schemas include:

* support for optional fields and default field values
* support for sum types aka algebraic data types or tagged unions
* options to select alternate representations than the default, e.g.
  use a JSON object rather than an array of pairs
