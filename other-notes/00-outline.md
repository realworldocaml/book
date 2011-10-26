# Proposal for Real World OCaml

[yminsky: can we really call it “Real World OCaml”?  It would be a
good name, but I don't know whose permission we'd need for it.
Another name that I was thinking of using was “Core OCaml”, which has
the pun going for it.]

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

1. **Introduction to OCaml**: A guided tour through the OCaml
   language, all done using the OCaml toplevel.  We'd cover all the
   basic language features in brief, including:
      - basic arithmetic expressions
      - defining variables
      - definint functions
      - simple higher-order functions
      - basic type inference
      - basic pattern matching
      - lists and tuples
      - records and variants
1. **Expressions, Variables and Functions**: 
      - Discuss the idea that OCaml is an expression-oriented
        language.  Explain the basic syntactic constructs and how they
        work.
      - Discuss how let binding and variable definitions work.
        Include
      - Discuss function definitions in more detail, explaining what
        labelled and optional arguments are and when you should use
        them, anonymous functions, and recursive functions.
1. **Tuples, Lists and Polymorphism**:
      - Explain polymorphism in more detail, both in terms of
        functions and datatypes.
      - This section will also give us an opportunity to explain lists
        in more detail, including the special syntax for Cons.
      - The value restriction
1. **Algebraic Data Types and Pattern Matching**:
      - Explain the interplay between product types and sum types.
      - define how unions can be defined with the example of a binary
        tree and an R/B tree.  Show how pattern matching can be used
        to work with algebraic data types. [yminsky: I'm a little
        concerned baout using binary trees as the motivating example.
        Variants are far more useful than the example suggests.  Maybe
        a more prosaic example is in order.]
      - Explain the option list types in terms of variants types.
      - Introduce polymorphic variants, show how they can be used to
        give you more flexibility (and explain the downsides)
1. **Mutable Programming**: 
      - OCaml code can be purely functional, or use side-effects and
        global variables. 
      - Introduce references and show how to use.  Explain how to
        write imperative code, including for and while loops and
        sequencing operations with semi-colons.
      - Discuss records in more detail, including an explanation of
        mutable record fields.  Mention that references are just an
        instance of this.
      - Discuss the various mutable datastructures including records,
        arrays, strings, and hashtables.
1. **Exceptions**: 
      - Explain the exceptions system, show how exceptions are defined
        and caught.
      - Explain the downsides of exceptions, and how and when to use
        them.
1. **Module Basics**:
      - A basic introduction to modules, how they show up in the file
        system, how and why you should use interfaces.
      - Tips for designing good module interfaces.
      - Effective use of modules, including interface components (like
        Comparable, Hashable, Sexpable in Core)
1. **Advanced Modules**:
      - Functors
      - First-class modules, using a plug-in system as the motivating
        example.
      - recursive modules [yminsky: Do we want to cover this?  I've
        personally never used recursive modules.]
1. **Files and Compilation Units**: [yminsky: maybe this section
   should be unified with "Module Basics" section somehow?]
      - How to organize a small realistic project, including
      - the role of ml/mli files
      - direct invocation of the compiler
      - setting up a simple build with ocamlbuild.
1. **I/O**:
      - Basic input and output.  Printf, and in/out channels.
      - reading and writing values using s-expressions, bin-prot and
        marshal.
1. **Concurrent Programming**
      - Covers Lwt or Async.  We still need to figure out which system
        to cover.  I'm pretty torn on this one.
1. **Objects**: the structurally typed object system. Two good
   examples of object usage are lablgtk and js_of_ocaml where they
   interop nicely. Perhaps do a simple windowing system using lablgtk
   here?
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
1. **Ocamlbuild**:
   [yminsky: I wonder if this is something we should cover to some
   degree early on.  It would be nice to get people to set up a
   trivial ocamlbuild setup in the very beginning, to do their
   building with.]

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

> Schedule: Doesn't need to be that fine grained, and I'll add some
> slush factor to it.
