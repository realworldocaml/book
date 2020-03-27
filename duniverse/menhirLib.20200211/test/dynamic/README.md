This directory contains several tests where Menhir is used to generate a
parser, where the parser is executed, and where the parser's output or
execution time is tested.

The following tests are currently performed:

* The subdirectory `semantics` contains a parser for arithmetic expressions, a
  variant of the `calc` demo. The parser is compiled in several different ways
  (code back-end; table back-end; table back-end, with inspection API). It is
  run on concrete inputs, and its output is compared against a reference
  output. The reference interpreter is also tested on symbolic inputs.

* The subdirectory `positions` also compiles a parser in several different
  ways (using Menhir with various options and `ocamlyacc`) and checks that
  all of them compute the same positions.

* The subdirectory `speed` measures the execution time of parsers produced by
  `ocamlyacc` and by Menhir's code back-end and table back-end. (This test
  always succeeds.)

These tests are normally run by typing `make test` in the root directory.

Because the speed test is time-consuming, as an exception, it is run by typing
`make speed` in the root directory.
