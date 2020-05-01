# Installation

## Requirements

You need OCaml (version 4.02.3 or later) and `dune` (version 2.0 or later).

## Compilation and Installation

Compile and install as follows:

```
  make all     # or: dune build @install
  make install # or: dune install
```

The executable file `menhir` and the libraries `MenhirLib` and `MenhirSdk` are
installed by `dune`. `dune` usually figures out by itself where they should be
installed. If desired, a `--prefix` option can be passed to `dune`.

## Coq support

If you wish to use Menhir's Coq back-end,
which produces verified parsers,
then you must install the Coq library `coq-menhirlib`.

This is normally done via the following commands:

```
  opam repo add coq-released https://coq.inria.fr/opam/released
  opam install coq-menhirlib
```

The library can also be manually installed as follows:

```
  cd coq-menhirlib
  make
  make install
```
