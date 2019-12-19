Real World OCaml v2
-------------------

This is the source code for the Real World OCaml 2nd edition, which
is still a work in progress.  The original edition was written by
Yaron Minsky, Anil Madhavapeddy and Jason Hickey, and the revised
edition is being lead by Yaron Minsky and Anil Madhavapeddy.  There
have been significant contributions to the revised tooling from
Ashish Agarwal, Jeremy Yallop, Frederic Bour, and Sander Spies.

An online snapshot of the development book is available from
<https://dev.realworldocaml.org>.  There is a Feedback pane on
each chapter which leads to a dedicated section on the OCaml
[discussion forum](https://discuss.ocaml.org) where you can register
broader feedback.  More specific issues such as typos can be
reported on the [issue tracker](https://github.com/realworldocaml/book/issues).

## Repository layout

Each chapter of the book sits in a separate subfolder of the `book/` directory.
The `README.md` file contains the text of the chapter, written in markdown.
Each ocaml or shell code block in the chapter is validated using
[mdx](https://github.com/realworldocaml/mdx). The more complex and structured
examples live in an `examples/` sub folder and mdx is used to keep the examples
and the chapter's code block in sync.

The `bin/` folder contains the OCaml scripts used to generate the books HTML
and PDF versions.

All of the code and examples are built using OCaml 4.08.1.

## Building

Here are the commands to build the website:

### Installing Dependencies

You can install system dependencies by running:

```
make depext
```

All OCaml dependencies are vendored in the `duniverse/` directory except
for the `dune` build system itself. It's preferable to use an empty opam switch
with only `dune` installed to avoid conflicts between the opam and local
libraries. To set up your RWO development environment you can run:

```
opam switch create rwo 4.08.1
opam install dune=2.0.0
```

### Generating the HTML

To generate the HTML pages:

```
make
```

The HTML pages are created in `_build/default/static/`.
Open `_build/default/static/index.html` to start browsing the
freshly built version of the book.

### Testing the code examples

It is possible to automatically test that
the [the code examples](./examples/code) files work fine. To check that shell
scripts and `.ml` files do what they are expected:

```
make test
```

This will run all the tests in "determinitic mode", which is suitable for the
CI and it will display the diff between what is expected and what is produced.

To accept the changes:

```
make promote
```

### Testing non-deterministic examples

A few code examples are not deterministic: for instance benchmarks. In this case,
there is a special command to run:

```
make test-all
```

To accept the changes:

```
make promote
```

## Upgrading or adding dependencies

RWO's dependencies are vendored using `duniverse`. If you want to upgrade them
to their latest availbale opam version you can run:

```
make duniverse-upgrade
```

Additionally, if you're working on the book and need a new package vendored, you
can simply add it to the `$DEPS` variable in the `Makefile` and run the above
command again.

It's possible that after upgrading you get some errors because vendored
dependencies use jbuild files instead of dune files and compatibility with those
has been dropped in dune 2. You can upgrade those using the following command:

```
dune upgrade --root duniverse/<package_name>.<version>
```
