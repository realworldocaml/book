# Installation

To work through Real World OCaml, you'll need the following software packages
installed:

* [OCaml](http://ocaml.org) version 4.1.0 or greater.  This book uses some tools that we've developed while writing it, and so you need to ensure that you have at least this version before trying the examples in the book.
* [OPAM](http://opam.ocaml.org) version 1.0 or greater.  This will give you access to all the OCaml libraries used in Real World OCaml.
* [Utop](https://github.com/diml/utop) is a modern interactive toplevel with command history, tab completion, and defaults that are tuned to work with the examples in Real World OCaml.

The easiest way to install OCaml is usually via the binary packages available
for many operating systems.  For day-to-day code development however, it's much
easier to use a source-code manager that lets you modify individual libraries
and automatically recompile all the dependencies.

An important difference between OCaml and scripting languages such as Python or
Ruby is the static type safety that means that you can't just mix-and-match
compiled libraries.  Interfaces are checked when libraries are compiled, so
when an interface is changed, all the dependent libraries must also be
recompiled.  Source-based package managers such as OPAM automate this process
for you and make development life much easier.

## Online Instructions

We've placed the installation instructions online to ensure they remain uptodate.
Please visit:

* <https://realworldocaml.org/install> and follow the instructions for your operating system.

* <https://github.com/realworldocaml> has all of the example code freely available under a public-domain-like license, so that you can copy them for use in your own projects.
