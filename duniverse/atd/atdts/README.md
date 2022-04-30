Atdts
==

Atdts takes type definitions in the ATD format and derives TypeScript
classes that can read and write JSON data. This saves the developer the
labor writing boilerplate that converts between dicts and classes.

This allows safe interoperability with other languages supported by
ATD such as OCaml, Java, Scala, or Python.

See the sample input type definitions
[everything.atd](test/atd-input/everything.atd) and
the TypeScript output [everything.ts](test/ts-expected/everything.ts).

Requirements
--

...

Documentation
--

* [Main documentation for
  atdts](https://atd.readthedocs.io/en/latest/atdts.html)
* Command-line documentation: `atdts --help`

Development notes
--

...

Contributing
--

Help is welcome and there are various ways to help:
* Add examples to the documentation
* File a GitHub issue to report a problem
* Pick an issue that [has the `target:typescript`
  label](https://github.com/ahrefs/atd/issues?q=is%3Aissue+is%3Aopen+label%3Atarget%3Atypescript)
  and implement a solution
