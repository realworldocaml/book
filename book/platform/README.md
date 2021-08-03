# The OCaml Platform

Every language, no matter how elegant, also needs a set of tools for the
working programmer to deal with editing, compiling, testing, documenting and
publishing your source code. The OCaml community has adopted a platform of
modern tools that help you specify your project metadata and interface it with
IDEs (such as Visual Studio Code), to generate API documentation, and also to
adopt modern software engineering practises such as continuous integration (CI)
and fuzz testing.

Now that you are familiar with the core OCaml language, this second part of the
book will covers tools and techniques for using OCaml at a larger scale.  We
will begin by illustrating the typical structure for an OCaml project and how
you might get started.

The only project metadata that is *required* to participate in
the OCaml ecosystem is an `opam` file present in your source tree.  Each `opam`
file defines a collection of OCaml libraries and executable binaries or application
data.  Each `opam` file can define dependencies on other packages, and includes
build and testing directions.  A collection of `opam` files can be stored in an
*opam repository* to create a package database, with a central one for the OCaml
ecosystem available at <https://github.com/ocaml/opam-repository>.
The official (but not exclusive) tool used for manipulating `opam` files is the
eponymous [opam package manager](https://opam.ocaml.org).  For the purposes of this
chapter, make sure you have a basic installation of opam on your system.


## A Hello World OCaml Project

Let's start by creating a sample OCaml project and navigating around it.  opam provides
a project generator known as `spin` that can be used to create various types of projects.

```sh dir=examples/correct/opam-spin-ls
$ opam spin ls

  bin
    Native project containing a binary

  c-bindings
    Bindings to a C library

  cli
    Command Line Interface releasable on Opam

  js
    Javascript application with Js_of_ocaml

  lib
    Library releasable on Opam

  ppx
    PPX library

```

Before we dive into any of these, we'll generate a tutorial using Spin's built-in
hello world project.

```sh dir=examples/correct/opam-spin-hello
$ opam spin hello my-first-ocaml -vvv

🏗️  Creating a new project from hello in my-first-ocaml
Done!

🎁  Installing packages globally. This might take a couple minutes.
opam-spin: [ERROR] The template generation failed:
The command make build did not run successfully: exited with code 2
[7]
```

## Setting up continuous integration

GitHub Actions
- Use setup-ocaml


A note on production binaries using DUNE_RELEASE

- edit note: “The warnings used for building” should now mention the dune ones not the core build ones
- result.t is meant to address this deficiency” -> talk about `results instead
blah blah


