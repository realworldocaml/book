# The OCaml Platform

So far in the book, we've gone through a number of techniques you can use to build real OCaml programs.
We'll now wrap up this part by examining the tools you can use for editing,
compiling, testing, documenting and publishing your own projects.

The OCaml community has adopted a platform of modern tools to specify your project
metadata and interface it with IDEs such as Visual Studio Code, to generate API
documentation, and also to adopt modern software engineering practises such as
continuous integration (CI) and testing.

## A Hello World OCaml Project

Let's start by creating a sample OCaml project and navigating around it.  opam
provides a project generator known as `spin` that can be used to create various
types of projects.

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

<!-- ```sh dir=examples/correct/opam-spin-hello
TODO need a way to stop spin from running the opam install commands
 -->

```
$ opam spin hello hello-world -vvv

🏗️  Creating a new project from hello in hello-world
Done!

🎁  Installing packages globally. This might take a couple minutes.
```

Spin will create a `hello-world` directory and populate it with a skeleton
OCaml project.  This sample project has all the metadata required for us to learn more
about the opam package manager and the dune build tool that we've used earlier in the book.
First use opam to install the dependencies required for our hello world project.

<!-- ```sh dir=examples/correct/hello-world -->
```
$ make deps
```

This will invoke the opam CLI to install the project dependencies and some useful
tools.  opam doesn't require any special user permissions and stores all of the 
files it installs in `~/.opam` (for global installations) and `_opam` in the
working directory for the project-local installations.  You can use `opam env`
to add the right directories to your local shell path:

```
$ eval $(opam env)
```

If you prefer not to modify your shell configuration, then you can also invoke
the build commands via `opam exec` to modify the path for the subcommand. This is
exactly what the various targets in the `Makefile` do.

### Structure of an OCaml project

Back in [Files Modules And Programs](files-modules-and-programs.html#files-modules-and-programs){data-type=xref},
we looked at what a simple program with a couple of OCaml modules looks like. Let's now look at the full set of files in our `hello-world` application to examine a more realistic project structure.

```
├── .gitignore
├── LICENSE
├── Makefile
├── README.md
├── bin
│   ├── dune
│   ├── main.ml
│   └── main.mli
├── dune
├── dune-project
├── hello.opam
├── lib
│   ├── dune
│   ├── hello.ml
│   └── hello.mli
└── test
    ├── dune
    └── hello_test.ml
```

Some of the files here may be familiar to you from using other programming
languages:

- the `Makefile` contains targets for common actions such as `all`, `build`, `test`
  or `clean`.  It's useful to read through this to see which underlying OCaml tools
  are being invoked.
- the `LICENSE` defines the terms under which your code is made available, and
  defaults to the permissive ISC license.
- a `README.md` is a Markdown-formatted introduction to your library or application.
- the `.gitignore` file contains the patterns for generated files from the OCaml
  tools so that they can be ignored by the Git version control software.
  If you're not familiar with using Git, look over one of the tutorials
  one such as GitHub's [git hello world](https://guides.github.com/activities/hello-world/).

The remainder of the files are either source code or metadata files. There are three layers of names used in every OCaml project:

- **OCaml modules:** the individual `ml` and `mli` files each define an *OCaml module*, named after the file. Modules names are what you refer to when writing OCaml code -- for example, `Hello` is the module defined in our project.
- **ocamlfind libraries:** one or more OCaml modules can be gathered together into an *ocamlfind library*, providing a convenient way to package up some dependencies with a single name -- in this case, the `hello` library. Although this example contains just the single `Hello` module , it is common to have multiple modules per library. You can query the installed libraries via `ocamlfind list` at your command prompt.
- **opam packages:** a set of ocamlfind libraries, binaries and application data can all be gathered together into an *opam package*, in this case `hello.opam`. This is what is installed when you eventually publish the package and another user types in `opam install hello`.

It is important to understand the difference between modules, ocamlfind libraries and opam packages, as you will use each of these at different points of your OCaml coding journey.  The root of a project is marked by a `dune-project` file (more on that later). We typically structure our project into subdirectories that contain the modules for a particular library or binary, with each directory containing a separate `dune` file with build instructions.  In our hello world example, we have:

- a `lib/` directory that builds a `hello` ocamlfind library.
- a `test/` directory that defines unit tests for the library.
- a `bin/` directory that uses the `hello` library to build a standalone application that can be executed from the command-line.

### Defining ocamlfind libraries

A project usually puts the business logic of the application into a library rather than directly into an executable binary, since this makes writing tests and documentation easier in addition to improving reusability.  Let's look at `lib/dune` in more detail:

```scheme
(library
 (name hello)
 (public_name hello)
 (libraries))
```

The `(name)` field defines the project-internal name for the compiled library, and the `(public_name)` field is what it will be called when installed system-wide. The choice of `(name)` defines the toplevel module exposed by this library, and every other module in the library will be exposed as a "wrapped" submodule of that toplevel module. In our example project `hello.ml` is exported as the `Hello` module since it's the project name, but if we added a file called `world.ml` into this directory the resulting module would be found in `Hello.World`. While private library names must adhere to OCaml's module naming convention, it's common practise to use dashes and dots in public library names.

### Writing test cases for a library

The `(libraries)` field in the `hello` dune file is empty since this is a standalone library. Our next step is to define a test case in `tests/dune` for our library.

```scheme
(test
 (name hello_test)
 (libraries alcotest hello))
```

The `(test)` field builds an executable binary that is run when you invoke `dune runtest`.  In this case, it uses the `tests/hello_test.ml` module to define the test cases and depends on the external `alcotest` library _and_ the 
locally defined `hello` library.  Once you run the tests, you can find the built artefacts in `_build/default/tests/` in your project checkout.  You can use all the tests you learnt about in [Testing](testing.html){data-type=xref} here, including inline tests.

### Building an executable program

Finally, we want to actually use our hello world from the command-line. This is defined in `bin/dune` in a very similar fashion to test cases.

```scheme
(executable
 (name main)
 (public_name hello)
 (libraries hello))
```

Much like libraries, the `(name)` field here has to adhere to OCaml module naming conventions, and the `public_name` field represents the binary name that is installed onto the system and just needs to be a valid Unix or Windows filename.

You can build and execute the command locally using `dune exec` and the public name of the executable:

```sh dir=examples/correct/hello-world
$ dune exec -- hello
Hello world!
```

## Setting up an integrated development environment

Now that we've seen the basic structure of the OCaml project, it's time to setup an integrated development environment. An IDE is particularly useful for use with OCaml due to the extra information you gain from the static type information present in the codebase. A good IDE will provide you with the facilities to browse interface documentation, see inferred types for code, and to jump to the definitions of external modules.

### Using Visual Studio Code

The recommended IDE for newcomers to OCaml is [Visual Studio Code](https://code.visualstudio.com) using the [OCaml Platform plugin](https://marketplace.visualstudio.com/items?itemName=ocamllabs.ocaml-platform).  The plugin uses the Language Server Protocol to communicate with your opam and dune environment. All you need to do is to install the OCaml LSP server via opam:

```
opam install ocaml-lsp-server
```

Once installed, the VSCode OCaml plugin will ask you which opam sandbox to use. Just the default one should be sufficient get you going with building and browsing your interfaces.

### Browsing interface documentation

The OCaml LSP server understands how to interface with dune and examine the built artefacts, so opening your local project in VS Code is sufficient to activate all the features.  Try navigating over to `bin/main.ml`, where you will see the invocation to the `hello` library.

<!-- $MDX file=examples/correct/hello-world/bin/main.ml -->
```
let () =
  let greeting = Hello.greet "world" in
  print_endline greeting
``` 

First perform a build of the project to generate the type annotation files. Then hover your mouse over the `Hello.greet` function -- you should see some documentation pop up about the function and its arguments.  This information comes from the _docstrings_ written into the `hello.mli` interface file in the library.

<!-- $DISABLEDMDX file=examples/correct/hello-world/lib/hello.mli -->
```
(** This is a docstring, as it starts with "(**", as opposed to normal comments
    that start with "(*".

    The top-most docstring of the module should contain a description of the
    module, what it does, how to use it, etc.

    The function-specific documentation located below the function signatures. *)

val greet : string -> string
(** This is the docstring for the [greet] function.

    A typical documentation for this function would be:

    Returns a greeting message.

    {4 Examples}

    {[ print_endline @@ greet "Jane" ]} *)
```

Documentation strings are parsed by the [odoc](https://github.com/ocaml/odoc) tool generate HTML and PDF documentation from a collection of opam packages.  If you intend your code to be used by anyone else (or indeed, by yourself a few months later) you should take the time to annotate your OCaml signature files with documentation.  An easy way to preview the HTML documentation is to build it locally with dune:

```skip
$ opam install odoc
$ dune build @doc
```

This will leave the HTML files in `_build/default/_doc/_html`, which you can view normally with a webbrowser.

### Autoformatting your source code

As you develop more OCaml code, you'll find it convenient to have it formatted to a common style.  The `ocamlformat` tool can help you do this easily from within VSCode.

```skip
$ echo 'version=0.19.0' > .ocamlformat
$ opam install ocamlformat.0.19.0
```

The `.ocamlformat` file controls the autoformatting options available, and fixes the version of the tool that is used. You can upgrade to a newer ocamlformat version whenever you want, but it is a manual process to avoid an upstream release autoreformatting your project code without your intervention.  You can examine the formatting options via `ocamlformat --help` -- most of the time the defaults should be fine.

Once you've got ocamlformat configured, you can either format your project from within VSCode (`shift-alt-F` being the default), or by running:

```skip
$ dune build @fmt
```

This will generate a set of reformatted files in the build directory, which you can accept with `dune promote` as you did earlier in the testing chapter.

## Publishing your code online

With your IDE set up you'll quickly develop useful OCaml code and want to share it with others.  We'll now go through how to define opam packages, set up continuous integration and publish your code.

### Defining opam packages

The only metadata file that is really _required_ to participate in
the open-source OCaml ecosystem is an `opam` file in your source tree.  Each `opam`
file defines an *opam package* -- a collection of OCaml libraries and executable binaries or application
data.  Each opam package can define dependencies on other opam packages, and includes
build and testing directions for your project. The `hello.opam` file in our sample project is quite easy to read:

<!-- $MDX file=examples/correct/hello-world/hello.opam -->
```
# This file is generated by dune, edit dune-project instead
opam-version: "2.0"
synopsis: "A short description of the project"
description: "A short description of the project"
maintainer: ["Your name"]
authors: ["Your name"]
license: "ISC"
homepage: "https://github.com/username/hello"
doc: "https://username.github.io/hello/"
bug-reports: "https://github.com/username/hello/issues"
depends: [
  "ocaml" {>= "4.08.0"}
  "dune"
  "alcotest" {with-test}
  "odoc" {with-doc}
]
build: [
  ["dune" "subst"] {pinned}
  [
    "dune"
    "build"
    "-p"
    name
    "-j"
    jobs
    "@install"
    "@runtest" {with-test}
    "@doc" {with-doc}
  ]
]
dev-repo: "git+https://github.com/username/hello.git"
```

The fields in here all represent project metadata ranging
from textual descriptions, to project URLs, to other opam package dependencies.
A collection of `opam` files can be stored in an
*opam repository* to create a package database, with a central one for the OCaml
ecosystem available at <https://github.com/ocaml/opam-repository>.
The official (but not exclusive) tool used for manipulating `opam` files is the
eponymous [opam package manager](https://opam.ocaml.org) that we've been using
throughout this book.

### Generating project metadata from dune

You don't need to write the opam file by hand -- instead, we can use
define our project metadata using the dune build system and have it autogenerated for us.
We've already been using the `dune` build tool from back in [Files Modules And Programs](files-modules-and-programs.html#files-modules-and-programs){data-type=xref}. We're now going to look at how to configure your project for use with dune.

The root directory of an OCaml project built by dune has a `dune-project` file that defines the project metadata. In our hello world example, it starts with:

```scheme
(lang dune 2.0)
```

The line above is the version of the syntax used in your build files, and _not_
the actual version of the `dune` binary.  One of the nicest features of dune is that
it is forwards-compatible with older metadata. By defining the version of
the dune language that you are currently using, _future_ versions of dune will do their best to emulate the current behaviour until you chose to upgrade your project.

The rest of the `dune-project` file defines other useful project metadata:

```scheme
(name hello)
(documentation "https://username.github.io/hello/")
(source (github username/hello))
(license ISC)
(authors "Your name")
(maintainers "Your name")
(generate_opam_files true)
```

The fields here should look familiar -- they were also present in the `hello.opam` file
above.  That's because dune can generate the opam packaging metadata files for you and
avoid duplication.  Go ahead and edit the metadata above, and then build the project with:

```skip
$ dune build
```

The build command will update the `hello.opam` file in your source tree as well,
keeping it in sync with your changes.   The final part of the `dune-project` file
contains dependency information for other packages your project depends on.

```scheme
(package
 (name hello)
 (synopsis "A short description of the project")
 (description "A short description of the project")
 (depends
  (ocaml (>= 4.08.0))
  (alcotest :with-test)
  (odoc :with-doc)))
```

The `(package)` stanza here refers to opam packages, both for the name and for the dependency
specifications.  This is in contrast to the `dune` files which refer to ocamlfind libraries,
since those represent the compilation units for OCaml code (whereas opam packages are broader
collections of package data).

Notice that the dependency specification can also include version information. One of the
key features of opam is that each repository contains multiple versions of the same package.
The opam CLI contains a constraint solver that will find versions of all dependencies that are
compatible with your current project.  When you add a dependency, you can therefore specify
lower and upper version bounds as required by your use of that package.  The `with-test` and
`with-doc` are further constraints that only add those dependencies for test and documentation
generation respectively.
 
Once you've defined your opam and dune dependencies, you can run various lint commands to
check that your metadata is consistent.

```skip
$ opam dune-lint
$ opam lint
```

The `opam-dune-lint` plugin will check that the ocamlfind libraries and opam packages
in your dune files match up, and offer to fix them up if it spots a mismatch. `opam lint`
runs additional checks on the opam files within your project.

## Publishing your code online

### Pushing to GitHub

### Setting up Continuous Integration

GitHub Actions
- Use setup-ocaml


A note on production binaries using DUNE_RELEASE

- edit note: “The warnings used for building” should now mention the dune ones not the core build ones
- result.t is meant to address this deficiency” -> talk about `results instead


