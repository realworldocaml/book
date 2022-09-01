ATD project - Static Types for Json APIs
==

[![CircleCI](https://circleci.com/gh/ahrefs/atd/tree/master.svg?style=svg)](https://circleci.com/gh/ahrefs/atd/tree/master)

ATD stands for Adaptable Type Definitions. It is a syntax for defining
cross-language data types. It is used as input to generate efficient
and type-safe serializers, deserializers and validators.

Target languages currently supported:
* Java: [atdj](atdj)
* OCaml, Bucklescript: [atdgen](atdgen)
* Python: [atdpy](atdpy)
* Scala: [atds](atds)
* TypeScript: [atdts](atdts)

All can installed with opam e.g.
```
$ opam install atdgen
```

Documentation
--

[here](http://atd.readthedocs.io/)

Contributing
--

The ATD suite of tools is developed and maintained by
volunteers&mdash;users like you.
[Various issues](https://github.com/ahrefs/atd/issues) are in need
of attention. If you'd like to contribute, please leave a comment on the
issue you're interested in, or create a new issue. Experienced
contributors will guide you as needed.

There are many simple ways of making a positive impact. For example,
you can...

* Use the software in your project.
* Give a demo to your colleagues.
* Share the passion on your blog.
* Tweet about what you're doing with atd.
* Report difficulties by creating new issues. We'll triage them.
* Ask questions on StackOverflow.
* Answer questions on
  [StackOverflow](https://stackoverflow.com/search?q=atdgen).
* Discuss usage on the [OCaml forums](https://discuss.ocaml.org/).
* Pick a [task](https://github.com/ahrefs/atd/issues) that's easy for you.

Check out in particular
[good first time issues](https://github.com/ahrefs/atd/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+time+issue%22)
and other issues with which
[we could use some
help](https://github.com/ahrefs/atd/issues?q=is%3Aissue+is%3Aopen+label%3A%22help+wanted%22).

For guidelines on how to contribute code, consult
[CONTRIBUTING.md](CONTRIBUTING.md).

Authors
--

<!-- The list of contributors was obtained with:
       git shortlog -s -n | cut -f2 | sed -e 's/^/* /'

     Let's try to refresh it once in a while.
-->
The ATD project started in 2010. Contributors include:

* Martin Jambon
* Rudi Grinberg
* Ivan Jager
* David Sheets
* Jeff Meister
* Carmelo Piccione
* oleksiy
* Louis Roché
* Daniel Weil
* Egor Chemokhonenko
* Gabriel Scherer
* Raman Varabets
* tzm
* Mathieu Baudet
* Oleksiy Golovko
* Rauan Mayemir
* John Billings
* Brendan Long
* Caio Wakamatsu
* Chris Yocum
* Pierre Boutillier
* Shon Feder
* Anurag Soni
* Arjun Ravi Narayan
* Asya-kawai
* Christophe Troestler
* Damien Doligez
* Daniel M
* François Pottier
* Javier Chavarri
* Stephane Legrand
* Vincent Bernardoff
* haoyang
* pmundkur
* ygrek

We distribute the source code under the terms of a [BSD license](LICENSE.md).
