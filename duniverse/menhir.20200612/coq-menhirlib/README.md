# A support library for verified Coq parsers produced by Menhir

The [Menhir](http://gallium.inria.fr/~fpottier/menhir/) parser generator,
in `--coq` mode, can produce [Coq](https://coq.inria.fr/) parsers.

These parsers must be linked against this library, which provides
both an interpreter (which allows running the generated parser) and
a validator (which allows verifying, at parser construction time,
that the generated parser is correct and complete with respect to
the grammar).

## Installation

To install the latest released version, use `opam install coq-menhirlib`.

To install from the sources, use `make` followed with `make install`.

## Authors

* [Jacques-Henri Jourdan](jacques-henri.jourdan@lri.fr)
