Yojson: JSON library for OCaml
==============================

[![Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Focaml-community%2Fyojson%2Fmaster&logo=ocaml)](https://ci.ocamllabs.io/github/ocaml-community/yojson)

This library parses JSON data into a nested OCaml tree data structure.


Library documentation
---------------------

Currently at https://ocaml-community.github.io/yojson/


Examples
--------

A simple example on how to parse JSON from a string literal.

```ocaml
let json_string = {|
  {"number" : 42,
   "string" : "yes",
   "list": ["for", "sure", 42]}|}
(* val json_string : string *)

let json = Yojson.Safe.from_string json_string
(* val json : Yojson.Safe.t *)

Format.printf "Parsed to %a" Yojson.Safe.pp json
```


Related tooling
---------------

`Yojson` is a pretty common choice for parsing JSON in OCaml, as such it is the
base for a number of tools and libraries that are built on top of it.

* [`ppx_deriving_yojson`](https://github.com/ocaml-ppx/ppx_deriving_yojson) to
  automatically generate code that converts between `Yojson.Safe.t` and custom
  OCaml types
* [`ppx_yojson_conv`](https://github.com/janestreet/ppx_yojson_conv), an
  alternative to `ppx_deriving_yojson` from Jane Street with different design
  decisions
* [`atd`](https://github.com/ahrefs/atd), generates mapping code from `.atd`
  specification files and can be used in multiple languages
* [`jsonm`](https://erratique.ch/software/jsonm) is an alternate JSON parser
  that parses JSON into a stream of items, so the complete data structure does
  not have to be in memory.

Help wanted
-----------

Yojson is developed and maintained by volunteers &mdash; users like you.
[Various issues](https://github.com/ocaml-community/yojson/issues) are in need
of attention. If you'd like to contribute, please leave a comment on the issue
you're interested in, or create a new issue. Experienced contributors will
guide you as needed.

There are many simple ways of making a positive impact. For example,
you can...

* Use the software in your project.
* Give a demo to your colleagues.
* Share the passion on your blog.
* Tweet about what you're doing with `Yojson`.
* Report difficulties by creating new issues. We'll triage them.
* Ask questions on StackOverflow.
* Answer questions on
  [StackOverflow](https://stackoverflow.com/search?q=yojson).
* Discuss usage on the [OCaml forums](https://discuss.ocaml.org/).
* Pick a [task](https://github.com/ocaml-community/yojson/issues) that's easy
  for you.

Check out in particular
[good first time issues](https://github.com/ocaml-community/yojson/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+time+issue%22)
and other issues with which
[we could use some help](https://github.com/ocaml-community/yojson/issues?q=is%3Aissue+is%3Aopen+label%3A%22help+wanted%22).


License
-------

`Yojson` is licensed under the 3-clause BSD license, see `LICENSE.md` for
details.
