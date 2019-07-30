Biniou
======

Biniou (pronounced "be new") is a binary data format designed for speed,
safety, ease of use and backward compatibility as protocols evolve.
Biniou is vastly equivalent to JSON in terms of functionality but allows
implementations several times faster (4 times faster than
[yojson](https://github.com/mjambon/yojson)), with
25-35% space savings.

Biniou data can be decoded into human-readable form without knowledge
of type definitions except for field and variant names which are
represented by 31-bit hashes. A program named `bdump` is provided for
routine visualization of biniou data files.

The program [atdgen](https://mjambon.github.io/atdgen-doc/)
is used to derive OCaml-Biniou serializers and
deserializers from type definitions.

Biniou format specification:
https://mjambon.github.io/atdgen-doc/biniou-format.txt
