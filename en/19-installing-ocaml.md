# Installation

The easiest way to install OCaml is via the binary packages available in
many operating systems.  For day-to-day code development, it is easier to
use a source-based manager that lets you recompile individual libraries
easily.

For the purposes of this book, we'll use the OPAM source manager.  There are
other alternatives available such as GODI and ODB, but not covered here.  Let's
get started with OPAM now, as you will need it to run the examples in the rest
of the book.  OPAM manages multiple simultaneous OCaml compiler and library
installations, tracks library versions across upgrades, and recompiles
dependencies automatically if they get out of date.

## OPAM Base Installation

To install OPAM, you will need a working OCaml installation to bootstrap the
package manager.  Once installed, all of the OPAM state is held in the
`$HOME/.opam` directory, and you can reinitialise it by deleting this directory
and starting over.

<important>
<title>OCamlfind and OPAM</title>

OPAM maintains multiple compiler and library installations, but this can clash
with a global installation of the `ocamlfind` tool.  Uninstall any existing
copies of `ocamlfind` before installing OPAM, and use the OPAM version instead.

</important>

#### MacOS X

The easiest way to install OCaml on MacOS X is via [Homebrew](http://github.com/mxcl/homebrew).

```
$ brew install ocaml
$ brew install pcre
$ brew install opam
```

The Perl-compatible Regular Expression library (PCRE) is used by the Core
suite.  It's a commonly used library that you may already have installed for
some other reason.

#### Linux

On Debian Linux, you should install OCaml via binary packages, and then install
the latest OPAM release from source.

```
$ sudo apt-get install build-essential m4 ocaml ocaml-native-compilers camlp4-extra git
$ tar -zxvf opam-<version>.tar.gz
$ cd opam-<version>.tar.gz
$ ./configure && make && sudo make install
```

On Fedora/RHEL...?

#### Windows

Investigate Protzenko's Windows installer.

### Using the OPAM top-level

All the OPAM state is held in the `.opam` directory in your home directory,
including compiler installations. You should never need to switch to an admin
user to install packages. 

```
$ opam init
$ opam install utop async core_extended
$ eval `opam config env`
```

This will initialise OPAM with the default package set from
`opam.ocamlpro.com`, and install the `utop` interactive top-level and the
`Async` library.  OPAM figures out the minimal set of dependencies required,
and installs those too.  The `eval` command is sets your `PATH` variable to
point to the current active compiler, and you should add this to your shell
`.profile` to run every time you open a new command shell.

### Switching compiler versions

The default compiler installed by OPAM uses the system OCaml installation. You
can use `opam switch` to swap between different compiler versions, or experiment
with a different set of libraries or new compiler versions. For instance, one
of the alternate compilers is patched to simplify the types that are output
in the top-level. You can switch it to by:

```
$ opam switch -list
$ opam switch 4.00.1+short-types
$ eval `opam config env`
$ opam install utop async_extra
```

The new compiler will be compiled and installed into
`~/.opam/4.00.1+short-types` and the libraries will be tracked separately from
your system installation.  You can have any number of compilers installed
simultaneously, but only one can be active at any time.

## Editing Environment

### Command Line

The `utop` tool provides a convenient interactive top-level, with full command
history, command macros and module name completion.  An `.ocamlinit` file in
your home directory will initialise `utop` with common libraries and syntax
extensions open, e.g.:

```ocaml
#use "topfind"
#camlp4o
#thread
#require "core.top";;
#require "async";;
open Core.Std
open Async.Std
```

TODO: the `.ocamlinit` handling in OPAM is being finalised and is tracked in [issue 185](https://github.com/OCamlPro/opam/issues/185).

### Editors

Emacs users have tuareg and [Typerex](http://www.typerex.org/).

Vim users can use the built-in style, and [ocaml-annot](http://github.com/avsm/ocaml-annot) may also be useful.

Eclipse plugins: which one is maintained?

## Developing with OPAM

Package listings are obtained by adding *remotes* that provide package
descriptions, installation instructions and URLs.
