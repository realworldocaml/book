Atdpy
==

Atdpy takes type definitions in the ATD format and derives Python
classes that can read and write JSON data. This saves the developer the
labor writing boilerplate that converts between dicts and classes.

This allows safe interoperability with other languages supported by
ATD such as OCaml, Java, or Scala.

See the sample input type definitions
[everything.atd](test/atd-input/everything.atd) and
the Python output [everything.py](test/python-expected/everything.py).

Requirements
--

Requirements for building and testing `atdpy`:
* Opam and dependencies installed from the [`atd` project root](..)
  with `make setup`.
* Python 3 (>= 3.7), mypy, pytest

Requirements for generating Python code:
* the `atdpy` executable

Requirements for running the generated Python code:
* Python >= 3.7
* the generated code (no need for a runtime library at this time)

Documentation
--

* [Main documentation for
  atdpy](https://atd.readthedocs.io/en/latest/atdpy.html)
* Command-line documentation: `atdpy --help`

Development notes
--

Build or rebuild with `make`. Test with `make test`. This requires
`pytest` which can be installed with

```
pip install pytest mypy
```

Running the tests is done from the `atdpy/` main folder with `make
test`.

We have two kinds of tests for atdpy:
* [unit tests](src/test) for testing internal OCaml code
* code generation and Python tests:
  * they generate Python code from ATD files and compare the Python output
    against the [expectations](python-expected).
    Updating the expected output files is done with `dune promote`
    (similar to `pytest --snapshot-update`).
  * the generated code is executed by some tests using `pytest`.

Contributing
--

Help is welcome and there are various ways to help:
* Add examples to the documentation
* File a GitHub issue to report a problem
* Pick an issue that [has the `target:python`
  label](https://github.com/ahrefs/atd/issues?q=is%3Aissue+is%3Aopen+label%3Atarget%3Apython)
  and implement a solution
