# Contributing

## Building and testing

`odoc-parser` uses [dune](https://dune.build/) to build and run tests. We use
inline expect tests with [ppx_expect](https://github.com/janestreet/ppx_expect) -
see the [test file](test/test.ml).

We require that the parser builds and tests pass on all versions of the OCaml
compiler from 4.02.3 onwards. This is tested on each pull request by
[ocaml-ci](https://github.com/ocurrent/ocaml-ci).

Build the project using

```sh
$ dune build
```

and test using

```sh
$ dune runtest
```

if the code has changed and the tests require updating, use `dune promote`.

