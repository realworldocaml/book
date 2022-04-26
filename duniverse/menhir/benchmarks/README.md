# Benchmarks

This directory contains benchmarks for Menhir.

The general shape of the benchmarks is specified in the `template` directory.

Every subdirectory of `parsers` contains a parser that we wish to benchmark.
At least two files are needed to specify such a parser:

* `parser.mly`, a parser with a start symbol named `main`;

* `lexer.mll`, a lexer that accepts a sentence presented as a sequence of
  symbolic token names, separated by whitespace. This lexer must define an
  exception named `ExnEOF` and raise this exception when it reaches the end of
  the input.

Running `make prepare` uses the contents of the directory `template` as a
template to create one directory `build/XXX` for each parser `parser/XXX`.

Running `make` runs the benchmarks.
