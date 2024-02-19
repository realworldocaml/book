Real World OCaml v3
-------------------

This is the source code for the Real World OCaml 3rd edition, which is
still a work in progress.  The original edition was written by Yaron
Minsky, Anil Madhavapeddy and Jason Hickey, and the 2nd and subsequent
editions are being led by Yaron Minsky and Anil Madhavapeddy.  There have been
significant contributions to the revised tooling from Ashish Agarwal,
Jeremy Yallop, Frederic Bour, and Sander Spies.

An online snapshot of the development book is available from
<https://dev.realworldocaml.org>.  There is a Feedback pane on each
chapter which leads to a dedicated section on the OCaml [discussion
forum](https://discuss.ocaml.org) where you can register broader
feedback.  More specific issues such as typos can be reported on the
[issue tracker](https://github.com/realworldocaml/book/issues).

## Repository layout

Each chapter of the book sits in a separate subfolder of the `book/`
directory.  The `README.md` file contains the text of the chapter,
written in markdown.  Each OCaml or shell code block in the chapter is
validated using [mdx](https://github.com/realworldocaml/mdx). The more
complex and structured examples live in an `examples/` sub folder and
mdx is used to keep the examples and the chapter's code block in sync.

The `bin/` folder contains the OCaml scripts used to generate the books HTML
and PDF versions.

All of the code and examples are built using OCaml 4.14.1.

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
opam switch create rwo 4.14.1
opam install dune=3.6.1
```

### Generating the HTML

To generate the HTML pages:

```
make
```

The HTML pages are created in `_build/default/static/`.
Open `_build/default/static/index.html` to start browsing the
freshly built version of the book.

#### WIP Chapters

It is possible to mark a chapter as WIP so that it won't be included in the main
website or PDF.  To do so, you can wrap your chapter in a
`(wip <chapter-folder>)` in the table of content file, `book/toc.scm`, e.g.:

You can run `dune build @site-wip` or `dune build @pdf-wip` to generate the
website or PDF versions of the book including all WIP chapters. You can find
them respectivey in `_build/default/static-wip/index.html` and
`_build/default/static-wip/book.pdf`.

Once the chapter is ready, you can simply replace `(wip <chapter-folder>)` with
`<chapter-folder>` in `book/toc.scm`.

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

A few code examples are not deterministic: for instance benchmarks. In
this case, there is a specific dune profile to use:

```
dune runtest --profile non-deterministic ...
```

To accept the changes:

```
make promote
```

### Standalone examples

Examples in each chapter's `examples/` folder are split between
`correct/` and `erroneous/`.  Each individual example is a valid
dune-project that lives in its own sub folder.  Examples that contain
errors on purpose, for instance to showcase some specific compile
errors, go into the latter. All other examples should go in
`correct/`.

Examples in the `correct/` folder are automatically built and tested
in the CI.  It's possible to build and test them individually using
the dune alias corresponding to the example folder name. For instance,
to build and test the example in
`book/imperative-programming/examples/correct/dictionary`, one can
run:

```
dune build @dictionary
```

Note that the `runtest` alias will also build and test examples so
running `make test` will build all of the book's examples.

When adding a new chapter, the example folder should have the
following structure:

```
examples/
├── correct/
├── dune
├── dune.inc
└── erroneous/
```

With the following dune file:

```
(data_only_dirs correct erroneous)

(rule
 (deps
  (source_tree ./))
 (action
  (with-stdout-to dune.gen
   (run rwo-examples-rules ./))))

(rule
 (alias runtest)
 (action (diff dune.inc dune.gen)))

(include dune.inc)
```

From that point forward, running `dune runtest` will generate the
right dune rules for each folder in `correct/`. When the rules change,
they must be accepted through promotion first.

Each example must explicitly define its external dependencies in a
`.rwo-example`. For instance, if your example requires `base` and
`core`, it must include the following `.rwo-example` file at its root:

```
(packages base core)
```

## Dependencies

RWO's dependencies are managed using the `opam-monorepo` plugin. The dependencies are expressed
in the `rwo.opam` opam file as they would be for any project. The plugin is used to generate a
`rwo.opam.locked` lockfile from this deps specification using the `opam monorepo lock`
command. Running `opam monorepo pull` will then fetch the sources locally into the `duniverse/`
folder so that rwo and its dependencies can all be built together in a single dune-workspace.

You can install it by running:
```
opam install opam-monorepo
```

Before running `opam-monorepo lock` it's important to have the proper opam configuration.
We need to both add the `opam-overlays` repo which contains dune port of some of our dependencies.
We also use a pinned version of ctypes until the dune-port is stable. To set these up, you can run:
```
opam repository add dune-opam-overlays git+https://github.com/dune-universe/opam-overlays.git
opam pin add ctypes.0.20.1+dune https://github.com/avsm/ocaml-ctypes.git#dune-port
opam pin add ctypes-foreign.0.20.1+dune https://github.com/avsm/ocaml-ctypes.git#dune-port
```

### Upgrading or adding dependencies

Before upgrading or adding any dependency, you should make sure they are up-to-date according to the
`rwo.opam` file by running:
```
opam monorepo lock
opam monorepo pull
```
and committing the resulting `duniverse/` and `rwo.opam.locked` if they changed. This preliminary
step will help distinguish how new dependency specifications actually impact the lockfile and
duniverse by splitting out the unrelated updates.

Once the above is done, you can modify the `dune-project` package definition by adding a new dependency
or modifying the bounds on an existing one. Then run:
```
dune build rwo.opam
opam monorepo lock
opam monorepo pull
```
to update the opam file, the lockfile and the duniverse folder.
