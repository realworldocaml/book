# odoc &nbsp; [![Build Status][travis-img]][travis]

[travis]: https://travis-ci.org/ocaml/odoc/branches
[travis-img]: https://api.travis-ci.org/ocaml/odoc.svg?branch=master

**odoc** is a documentation generator for OCaml. It reads *doc comments* ,
delimited with `(** ... *)`, and outputs HTML.
See [example output][odig-sample].

[odig-sample]: https://b0-system.github.io/odig/doc/index.html

Text inside doc comments is marked up in ocamldoc syntax:

```ocaml
val compare : string -> string -> int
(** [compare s1 s2] compares [s1] and [s2] in {e lexicographic} order. The
    result is negative if [s1] precedes [s2], positive if [s1] follows [s2],
    and zero if [s1] and [s2] are equal. *)
```

The syntax reference is [here][comment-syntax]. There is also an
[explanation][comment-location] of how to attach comments to specific types,
values, and other elements in your program.

[comment-syntax]: http://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html#sec357
[comment-location]: http://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html#sec352

<br/>

odoc's main advantage over ocamldoc is an accurate cross-referencer, which
handles the complexity of the OCaml module system. odoc also offers a good
opportunity to improve HTML output compared to ocamldoc, but this is very much
a work in progress :)

<br/>

## Using `odoc` with OCaml

To install odoc with opam:

```
opam install odoc
```

If you want to use odoc on the packages you have installed in your
opam switch type:

```
opam install ocaml-manual odig
odig doc
```

When you are developing the easiest way to use odoc right now is by
having Dune drive it. This command should work in most Dune projects
out of the box:

```
dune build @doc
```

The generated docs can then be found locally at
`./_build/default/_doc/_html/index.html`.

<br/>

## Using `odoc` with BuckleScript/Reason

You can use the [`bsdoc`](https://reuniverse.github.io/bsdoc) npm package to use
`odoc` in your BuckleScript projects.

<br/>

## Contact

odoc is most discussed on [discuss.ocaml.org's Ecosystem category][discourse] with the `odoc` tag.
Please also don't hesitate to [open an issue][issues].

<br/>

## Contributing

Any [question asked](#contact), [issue opened][issues], feedback offered, is a
contribution to odoc, and the project and all its users are thankful :) If
you'd like to contribute code specifically, you may find the guide in
[`CONTRIBUTING.md`][contributing.md] helpful. Also see the [roadmap][roadmap]
there. If anything is missing from it, please don't hesitate to
[reach out](#contact).

[discourse]: https://discuss.ocaml.org/c/eco
[issues]: https://github.com/ocaml/odoc/issues/new
[contributing.md]: https://github.com/ocaml/odoc/blob/master/CONTRIBUTING.md#readme
[roadmap]: https://github.com/ocaml/odoc/blob/master/CONTRIBUTING.md#Roadmap
