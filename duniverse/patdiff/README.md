Patdiff - colored patience diffs with word-level refinement
===========================================================

Patdiff is an OCaml implementation of Bram Cohen's patience diff algorithm,
with a few extra conveniences for comparing code and config files:

**outputs plain ASCII, ANSI color codes, or HTML**

**optional semantic diffing of numbers**

![screenshot](./doc/float-tolerance.png)

**good word-level diffing out of the box**

![screenshot](./doc/refined.png)

And of course all the usual features:

- recursive diffing of directories
- extensively configurable output (markers, colors, location format, context)
- whitespace-aware diffing

## Installation

```sh
opam install patdiff
```

See <a href="https://github.com/janestreet/patdiff/blob/master/patdiff.opam" target="_blank">
here</a> for Patdiff's opam package file.
## Compiling from source

To build patdiff for local development, install its dependencies:

```sh
git clone https://github.com/janestreet/patdiff
cd patdiff
opam install --deps-only .
```

To compile, and optionally install, patdiff:

```sh
make
make install
```

## Usage

```sh
patdiff old-file new-file
```

If you don't supply any arguments to patdiff, it will read diff-like
text from stdin and color it in the normal patdiff way.

The file `~/.patdiff` is used as a config file if it exists.  You can
write a sample config with the `-make-config` flag.

## patdiff-git-wrapper

A simple [wrapper][patdiff-git-wrapper] is provided for using patdiff
as git's "external diff" tool.  You can enable it with:

    export GIT_EXTERNAL_DIFF=$(command -v patdiff-git-wrapper)

or

    git config --global diff.external $(command -v patdiff-git-wrapper)

[patdiff-git-wrapper]: https://github.com/janestreet/patdiff/blob/master/bin/patdiff-git-wrapper

## Documentation

More docs, including detailed API docs, are available
<a href="https://ocaml.janestreet.com/ocaml-core/latest/doc/patdiff/index.html" target="_blank">here</a>.
