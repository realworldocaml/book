# Installation

*avsm: these are just notes so far until we decide on firm recommendations for all these*

## Base Installation

### MacOS X

Homebrew is probably the best solution here.

### Windows

Protzenko's Windows installer.

### Linux

Debian/Ubuntu packages.
RHEL/Fedora RPMs.

## Useful Libraries

Core, Lwt, mainly.
Mention OPAM and OASIS-db, whichever works.
Many packages maintained on github these days.

## Environment

### Command Line

[rlwrap](http://utopia.knoware.nl/~hlub/uck/rlwrap/) provides line-editing support.
An `.ocamlinit` file in your home directory will set up your environment with useful libraries and syntax extensions open, e.g.:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
#use "topfind";;
#load "dynlink.cma";;
#camlp4o;;
#require "lwt.syntax";;
#require "lwt.unix";;
open Lwt;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Editors

Emacs users have tuareg and [Typerex](http://www.typerex.org/).

Vim users can use the built-in style, and [ocaml-annot](http://github.com/avsm/ocaml-annot) may also be useful.

Eclipse plugins: which one is maintained?

