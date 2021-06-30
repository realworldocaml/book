# Ppxlib - Meta-programming for OCaml

[![Travis status][travis-img]][travis] [![AppVeyor status][appveyor-img]][appveyor]

[travis]:         https://travis-ci.org/ocaml-ppx/ppxlib
[travis-img]:     https://travis-ci.org/ocaml-ppx/ppxlib.svg?branch=master
[appveyor]:       https://ci.appveyor.com/project/diml/ppxlib/branch/master
[appveyor-img]:   https://ci.appveyor.com/api/projects/status/bogbsm33uvh083jx?svg=true

[User manual][man]

# Overview

Ppxlib is the standard library for ppx rewriters and other programs
that manipulate the in-memory reprensation of OCaml programs, a.k.a
the "Parsetree".

It also comes bundled with two ppx rewriters that are commonly used to
write tools that manipulate and/or generate Parsetree values;
`ppxlib.metaquot` which allows to construct Parsetree values using the
OCaml syntax directly and `ppxlib.traverse` which provides various
ways of automatically traversing values of a given type, in particular
allowing to inject a complex structured value into generated code.

For more information about ppxlib and how to use it, please consult the
[user manual][man].

# What is the relation between ppxlib and other ppx libraries?

The ppx world has a long historied history, and if you look around you
may find other projects that offer functionalities similar to
ppxlib. The following [blog post][future-of-ppx] gives a good overview
of the various libraries that have been developed over time. At this
point, ppxlib is considered as the de facto library for writing ppx
rewriters.

# History of the project

This repository was created by merging several older smaller projects
that were developed at Jane Street. See [the history](HISTORY.md) for
more details.

[man]:           http://ppxlib.readthedocs.io/
[future-of-ppx]: https://discuss.ocaml.org/t/the-future-of-ppx/3766
