# Proposal for Real World OCaml

[yminsky: can we really call it “Real World OCaml”?  It would be a
good name, I think.  Another name that I was thinking of using was
“Core OCaml”, which has the pun going for it.]

## Outline

> Chapter titles and a paragraph on each chapter are great.

Some thoughts on structure: the current draft is based around
exercises, whereas O’Reilly books are usually more practical and
“recipe” oriented. So we should probably have sections on parsing
(e.g. JSON is a good one), network programming (likely with Async),
and GUI programming (a good example of objects too).  Ideally, we
could talk about the basics early, and introduce more advanced
concepts such as modules, objects and the FFI along with some of these
recipes.  In Real World Haskell, the appendices are reserved for
installation details on the online version (need to check what the
printed copy looks like).  Installation: common mechanisms to install
OCaml on Linux, Windows and MacOS X, as well as editor setup (Emacs,
Vim) and the interactive REPL.

### Part I : Language and Libraries

1. **Expressions, Variables and Functions**: introduce the basic OCaml
   types and syntax via expressions. Then discuss let bindings and
   functions, along with a demonstration of type inference in the
   REPL. Finally, show higher-order functions and labelled/optional
   arguments. [yminsky: I’ve been playing around with text for just
   such a section.  One thing to notice is that it’s pretty limiting
   to talk about functions, expressions and variables without even a
   list type.  You can get a few pages in that way, but at some point,
   you need something more interesting to work with.  I wonder if we
   could have an “OCaml walkthrough” section that takes you up through
   a bunch of the basics, including simple versions of all the
   material in 2-4, as a kind of broad overview, and then we could go
   into more detail in something like the order you suggest.]
1. **Pattern Matching**: the function and match operators, along with
   examples of exhaustiveness checking.
1. **Tuples, Lists, and Polymorphism**: show how to build up sets of
   values using tuples, and lists, as well as how to write generic
   functions that operate over them using the built-in operators.
1. **Algebraic Data Types**: define how unions can be defined with the
   example of a binary tree and an R/B tree. Then show how polymorphic
   variants can be used for even more open types.
1. **Mutable Data Structures**: OCaml code can be purely functional, or
   use side-effects and global variables. Introduce references and
   mutable structures such as the Queue built using them. Then discuss
   records, arrays, strings, and the built-in hashtable.
1. **Exceptions**: show how exceptions are defined and caught. XXX this
   interacts badly with Async/Lwt, so need to consider how those are
   introduced.
1. **I/O**: currently uses channels, but should we talk about this via
   Core/Async only? [yminsky: my inclination is to think that async
   should be presented when we get to network programming, but that
   ordinary file I/O at least should be covered without Async.  That
   said, there should be a chapter on Async.]
1. **Files and Compilation Units**: how to invoke the compiler and the
   role ml/mli files. This should possibly go earlier.
1. **Module System**: basic modules, example of Map. Then functors,
   first-class modules and recursive modules, along with uses for them
   (e.g. a plugin system).
1. **Objects**: the structurally typed object system. Two good examples of
   object usage are lablgtk and js_of_ocaml where they interop
   nicely. Perhaps do a simple windowing system using lablgtk here?
1. **Classes and Inheritance**: multiple inheritance and polymorphic
   classes. this is a beefy chapter, so I wonder about space
   constraints.

### Part II: Tools and Internals

This section is now about the internals of OCaml and helper tools:

1. **Foreign Function Interface**: example of how to bind a library, and
   common pitfalls.
1. **Tuning the Runtime**: brief description of the heap representation of
   values, and GC profiling and tuning.
1. **Camlp4**: a few examples of camlp4 tools and
   quotations/antiquotations (the XML parser might be quite a good one
   here).
1. **Ocamlbuild**

## About

> What the book is about, who needs to read it, and why. This should
> be a couple of paragraphs.

OCaml is a modern functional programming language with strong typing,
fast native code output, automatic type inference and a wide array of
mature libraries. This book introduces the basics of the language and
how to set up a development environment, and includes recipes for many
real-world tasks using the Core standard library. It also covers more
advanced topics such as the module system, foreign-function interface,
macro language and the ocamlbuild system.

The book is aimed at the working programmer interested in using OCaml
to solve day-to-day problems such as data processing, numerical
computation, system scripting, or database-driven web applications.
OCaml blends imperative, functional and object-oriented programming
styles in one language, making it a pragmatic and fun choice for
writing fast, succinct and readable systems code.

## Competition

> Competing books: what are they, and why is this better? This should
> be rather easy.

- Practical OCaml
- Developing Application with Objective Caml
- [OCaml for Scientists](http://www.ffconsultancy.com/products/ocaml_for_scientists/index.html)

## Schedule

> Schedule: Doesn't need to be that fine grained, and I'll add some slush factor to it.
