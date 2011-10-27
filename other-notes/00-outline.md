# Proposal for Real World OCaml

## Outline

### Examples

Some ideas for examples to integrate into the text.

- A log-file parser, maybe for Apache log files, or somesuch? it's a
  simple and common task, and could be done with a small amount of
  code.
- The JSON library from Real World Haskell is a nice kind of example.
  Instead of implementing a parser, he only implements a datatype for
  JSON values and some pretty printers.  That all seems far more
  tractable than implementing a parser.  Doing that for JSON or
  another format would be nice.
- An SNTP client.  I have one of these that I wrote a while back.
  It's dead simple, and I think could make a nice example.  That said,
  it does require some networking-foo.
- An ASCII pretty-printer for tabular data.  It's a nice example of
  how to design a clean OCaml library.

Another thing that might be fun to point people at is Bench.  It's a
simple benchmarking library, with much of its ideas cribbed from
Criterion.  

I'd like to have some example sections where we work through a
complete design.  In the first of those sections, we should do a basic
ocamlbuild setup so we can get people building complete, compiling
programs.

We should evenutally delete this section, and integrate the ideas we
want to keep into the outline below.

### Part I : Core OCaml

1. **A Guided Tour**: A guided tour through the language, all done
   using the interactive toplevel.  We'd cover all the basic language
   features in brief, including:
      - basic arithmetic expressions
      - defining variables and functions
      - simple higher-order functions
      - basic type inference
      - basic pattern matching
      - lists and tuples
      - records and variants
1. **Expressions, Variables and Functions**: 
      - Discuss the idea that OCaml is an expression-oriented
        language.  Explain the basic syntactic constructs and how they
        work.
      - Explain let binding and variable definitions
      - A more detailed discussion of functions, covering:
          - anonymous functions
          - labelled and optional arguments are and when you should use
            them
          - recursive and mutually recursive functions.
1. **Lists, Options and Pattern-matching**:
      - Lists and Options are important datatypes in OCaml, and this
        chapter will explain them in detail.
      - Lists are a great way of understanding pattern-matching, and
        we can implement a number of simple algorithms on lists as a
        way of showing off the pattern-matching system, and the
        correctness checks it provides.
1. **Algebraic Data Types**:
      - Explain the interplay between product types and sum types.
      - Explain the option and list types as variants.
      - Describe a somewhat more complex example using a recursive
        datatype, like a binary expression tree with a simple
        simplifier.
      - Introduce polymorphic variants, show how they can be used to
        give you more flexibility.
1. **Error Handling**: 
      - Explain the exceptions system, show how exceptions are
        defined, thrown and caught.
      - Explain the downsides of exceptions, and how and when to use
        them.
      - Explain how to do error handling with values Option and
        Result.
1. **Programming with Mutation**:
      - OCaml code can be purely functional, or use side-effects.
        This chapter will be about how to program with side-effects in
        OCaml.
      - Introduce references and show how to use them.  Describe
        OCaml's support for imperative code, including for and while
        loops and sequencing operations with semicolons.
      - Discuss records in more detail, including an explanation of
        mutable record fields.
      - Discuss OCaml's other mutable datastructures including arrays,
        strings, and hashtables.
1. **Modules**:
      - A basic introduction to modules, how they show up in the file
        system, how and why you should use interfaces.
      - Tips for designing good module interfaces.
      - Effective use of modules, including interface components (like
        Comparable, Hashable, Sexpable in Core)
      - How modules connect to files, the role of ml/mli files.
1. **I/O**:
      - Basic input and output.  Printf, and in/out channels.
      - reading and writing values using s-expressions, bin-prot and
        marshal.

### Part II: Advanced Topics

1. **Advanced Modules**:
      - Functors
      - First-class modules, using a plug-in system as the motivating
        example.
      - recursive modules *[yminsky: Do we want to cover this?  I've
        personally never used recursive modules.]*
1. **Concurrent Programming**
      - Covers Lwt or Async.  We still need to figure out which system
        to cover.  I'm pretty torn on this one.
1. **Syntax Extensions**
      - The syntax extensions that come with Core, and how to use
        them.  
      - The key ones to cover are: sexplib, fieldslib, variantslib.
      - Maybe also, bin-prot and pa_compare
1. **Objects**: the structurally typed object system. Two good
   examples of object usage are lablgtk and js_of_ocaml where they
   interop nicely. Perhaps do a simple windowing system using lablgtk
   here?
1. **Classes and Inheritance**: multiple inheritance and polymorphic
   classes. *[anil: this is a beefy chapter, so I wonder about space
   constraints.]*

### Part III: Tools and Internals

This section is now about the internals of OCaml and helper tools:

1. **Tuning the Runtime**: brief description of the heap representation of
   values, and GC profiling and tuning.
1. **Foreign Function Interface**: example of how to bind a library, and
   common pitfalls.
1. **Camlp4**: a few examples of camlp4 tools and
   quotations/antiquotations (the XML parser might be quite a good one
   here).
1. **Ocamlbuild**: Cover more in depth how to set up a project with
   ocamlbuild, including the writing of ocamlbuild plugins.

### Appendix: Installation and Configuration

This section will cover how to set up OCaml and Core on UNIX, MacOS
and Windows.  There will be a link to an on-line version of this data
that can be kept up-to-date.  Also, information will be given on how
to set up an editor and a the interactive toplevel.

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

### Practical OCaml

This book has a great title that goes along with a brand that includes
other good books like Practical Common Lisp.  Sadly, Practical OCaml
is a disaster.  The author was not particularly adept at OCaml and it
shows.  The book is packed full of bad advice, unidiomatic examples,
and poor writing.  One of our motivations for writing this book is so
that people interested in OCaml don't end up finding and reading
Practical OCaml, and ending up with a bad taste in their mouths.

###  Developing Application with Objective Caml



### [OCaml for Scientists](http://www.ffconsultancy.com/products/ocaml_for_scientists/index.html)

## Schedule

> Schedule: Doesn't need to be that fine grained, and I'll add some
> slush factor to it.
