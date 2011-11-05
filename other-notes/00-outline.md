# Proposal for Real World OCaml

## Outline

### Part I : Core OCaml

1. **A Guided Tour**: A guided tour through the language, all done
   using the interactive toplevel.  We'd cover all the basic language
   features in brief, including:
      - basic arithmetic expressions
      - defining variables and functions
      - simple higher-order functions
      - basic type inference
      - basic pattern matching
      - tuples, options and lists
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
          - lexical scoping.
      - Eager evaluation, and the `lazy` keyword.
1. **Lists, Options and Pattern-matching**:
      - Lists and Options are important datatypes in OCaml, and this
        chapter will explain them in detail.
      - Lists are a great way of understanding pattern-matching, and
        we can implement a number of simple algorithms on lists as a
        way of showing off the pattern-matching system, and the
        correctness checks it provides.
      - *Example*: an ASCII pretty printer for tabular data, using
        lists and pattern matching.
1. **Algebraic Data Types**:
      - Explain the interplay between product types and sum types.
      - Explain the option and list types and bool as variants.
      - Describe a somewhat more complex example using a recursive
        datatype, like a binary expression tree with a simple
        simplifier.
      - Introduce polymorphic variants, show how they can be used to
        give you more flexibility.
      - *Example*: define a full JSON definition and pretty printer
        using pattern matching.
1. **Error Handling**: 
      - Explain the exceptions system, show how exceptions are
        defined, thrown and caught.
      - Explain the downsides of exceptions, and how and when to use
        them.
      - Explain how to do more explicit error handling using the
        `Option` and `Result` types.
1. **Programming with Mutation**:
      - OCaml code can be purely functional, or use side-effects.
        This chapter will be about how to program with side-effects in
        OCaml.
      - Introduce references.  Describe OCaml's support for imperative
        code, including `for` and `while` loops and sequencing
        operations with semicolons.
      - Discuss records in more detail, including an explanation of
        mutable record fields.
      - Discuss OCaml's other mutable datastructures including arrays,
        strings, and hashtables.
1. **Modules**:
      - A basic introduction to modules, how they show up in the file
        system, how and why you should use interfaces.
      - *Example*: define a module type for partial ordering and vector
        clocks, and a driver to run it from the top-level.
      - Tips for designing good module interfaces.
      - Effective use of modules, including interface components (like
        `Comparable`, `Hashable`, `Sexpable` in Core)
      - How modules connect to files, the role of ml/mli files.
      - *Example*: use `Sexpable` to convert a vector clock into a wire
        representation.
1. **Synchronous I/O**:
      - Basic input and output.  `Printf`, and in/out channels.
      - Explain reading and writing values using s-expressions, bin-prot 
        and marshal.
      - Introduce the `Unix` module (which also works on Windows), and
        use it to create a network echo server.
      - *Example*: build an SNTP time synchronisation client using the
        `Unix` module and the `bitstring` library.
1. **Syntax Extensions**
      - The syntax extensions that come with Core, and how to use them
        and inspect the intermediate code that they generate.
      - The ones to cover are:
         - `sexplib` for serialising a value to an s-expression string
         - `fieldslib` and `variantslib` for providing first-class
           values for interacting with record fields and variant
           constructors and for iterating over them.
         - `bin-prot` for efficient binary serialization
         - `pa_compare` for efficient type-specific comparison

### Part II: Advanced Topics

1. **Advanced Modules**:
      - Using functors to reuse module code and improve abstraction.
      - *Example*: implement a heap module type, and binomial and leftist
        implementations of that type.
      - First-class modules, using a plug-in system as the motivating
        example.
      - Sharing constraints. *[jyh: This is pretty important, we might want
        it in basic modules.]* *[avsm: it does need functors or first-class
        modules to be explained first, so might be best left here?]*
      - recursive modules
      - How to define generalised algebraic data types (GADTs), and some
        example uses for them (e.g. a type-safe interpreter).
1. **Concurrent Programming**
      - Describe lightweight cooperative threads and their use for building
        event-driven systems. Introduce `Lwt` and basic primitives such
        as `bind` and `join`. Explain how to use the syntax extension.
      - *Example*: cooperative coin flipping with *n* threads.
      - Communicating between threads via `MVar`, a cooperative `Mutex`,
        and the `Stream` module.
      - *Example*: implement binomial options pricing via a cooperative 
        computation pipeline; an example of dynamic programming.
      - The `Lwt_unix` bindings for building cooperative network servers.
      - *Example*: port the earlier SNTP time client to be cooperatively
        threaded.
      - Explain the implications for debugging, backtraces and exceptions 
        when using cooperative threading.
      - *Example*: build a distributed in-memory key/value object cache 
        that stores JSON or s-expressions. Show how the Postgresql bindings
        can make this into a persistent store.
1. **Objects**:
      - Explain the basics of the structurally typed object system, and
        how object signatures and subtyping work. Objects use dynamic
        dispatch, and hence can be more flexible in some situations than 
        modules.
      - Show how classes and inheritance work, and their interaction with
        structural typing.
      - *Example*: use the `camlgraphics` package to build a object-oriented
        graphing module for tabular data.
      - Discuss the design difference between classes and ADTs (adding a
        new variant vs. adding a new method).
      - Objects are a powerful way to bridge to other languages such as 
        Javascript. Describe how `js_of_ocaml` works.
      - *Example*: use `js_of_ocaml` to build a Canvas version of the earlier
        graphing library.

### Part III: Tools and Internals

This section is now about the internals of OCaml and helper tools:

1. **Tuning the Runtime**:
      - brief description of the heap representation of values
      - the `GC` and `Weak` modules for tuning and creating GC-friendly
        data structures.
      - profiling and tuning performance via `ocamlprof`, `gprof` 
        and `oprofile`.
1. **Foreign Function Interface**: example of how to bind a library, and
   common pitfalls.
1. **Camlp4**: a few examples of `camlp4` tools and
   quotations/antiquotations 
    - *Example*: an embedded XML syntax.
1. **OCamlbuild**: Cover more in depth how to set up a project with
   ocamlbuild, including the writing of ocamlbuild plugins.
1. **ocamllex/ocamlyacc**
    - Explain how to parsing text into tokens and types using
      the `ocamllex` and `ocamlyacc` tools. Also mention useful 
      alternatives such as Menhir.
    - *Example*: write a JSON parser.


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

### Practical OCaml

This book has a great title that goes along with a brand that includes
other good books like Practical Common Lisp.  Sadly, Practical OCaml
is a disaster.  The author was not particularly adept at OCaml and it
shows.  The book is packed full of bad advice, unidiomatic examples,
and poor writing.  One of our motivations for writing this book is so
that people interested in OCaml don't end up finding and reading
Practical OCaml, and ending up with a bad taste in their mouths.

###  Developing Applications with Objective Caml

This book was originally written in French, and translated to English by the
community in 2007. It is the best available introduction to OCaml at present,
but is a little dated. It does not include many of the modern language features
features and a standard library such as Core. 

### [OCaml for Scientists](http://www.ffconsultancy.com/products/ocaml_for_scientists/index.html)

This book is written in 2005 for a scientific programmer seeking relief from
FORTRAN and C++, and includes examples on building numerical code and
OpenGL-based visualisation using OCaml. The book is self-published and
expensive (£85), and is also not as well-rounded as our proposed ``Real World
OCaml''. We also describe how to build scalable concurrent network services,
use database bindings, build foreign-function interfaces, and use the Core
standard library for rapid development.

## Schedule

We plan to develop Real World OCaml using an open development model, with feedback
from the community via a website where they can leave comments on each chapter
of the book. This is on the advice from the authors of Real World Haskell, who 
were very satisfied with the success of that method.

We have therefore registered `realworldocaml.org`, and the schedule below leaves
time for us to regularly update the website with chapter drafts and parse the
feedback.

- *March 2012*: website infrastructure live for `realworldocaml.org`, with the
  authors using it privately to upload drafts and collaborate on writing.
- *June 2012*: complete outline of all the chapters, with some text in place
  to clarify book flow. Give our advisory group private access to the website
  to deliver early private comments.
- *November 2012*: early draft chapters of the introduction and tools chapters,
  and open the website to the community for feedback.
- *March 2013*: completed draft content of all chapters including examples.
- *June 2013*: finished book for first-edition publication.

# People

## About the Authors

### Jason Hickey

Dr. Jason Hickey is a Software Engineer at Google Inc. in Mountain
View, California.  He is part of the team that designs and develops
the global computing infrastructure used to support Google services,
including the software systems for managing and scheduling massively
distributed computing resources.

Prior to joining Google, Dr. Hickey was an Assistant Professor of
Computer Science at Caltech.  He received the B.S. in Electrical
Engineering from Caltech; the M.S. in Electrical Engineering from
Cornell University; and the Ph.D. in Computer Science from Cornell
University.  His research at Caltech was in reliable and
fault-tolerant computing systems, including programming language
design, formal methods, and new models of distributed computation.  He
is the author of the MetaPRL system, a logical framework for design
and analysis of large software systems; OMake, an advanced build
system for large software projects.  He is the author of the textbook,
An Introduction to Objective Caml (unpublished).

### Anil Madhavapeddy

Dr. Anil Madhavapeddy is a Senior Research Fellow at the University of
Cambridge, based in the Systems Research Group. He was on the original team at
Cambridge that developed the Xen hypervisor, and helped develop an
industry-leading cloud management toolstack written entirely in OCaml. This
XenServer product has been deployed on hundreds of thousands of physical hosts,
and drives critical infrastructure for many Fortune 500 companies.

Prior to obtaining his PhD in 2006 from the University of Cambridge, Dr.
Madhavapeddy had a diverse background in industry at Network Appliance, NASA
and Internet Vision.  In addition to professional and academic activities, Dr.
Madhavapeddy is an active member of the open-source development community with
the OpenBSD operating system, is co-chair of the Commercial Uses of Functional
Programming workshop, and serves on the boards of startup companies such as
Ashima Arts where OCaml is extensively used.

### Yaron Minsky

Yaron Minsky heads the Technology group at Jane Street, a proprietary
trading firm that is the largest industrial user of OCaml.  He was
responsible for introducing OCaml to the company and for managing the
company's transition to using OCaml for all of its core
infrastructure.  Today, billions of dollars worth of securities
transactions flow each day through those systems.

Yaron obtained his PhD in Computer Science from Cornell University,
where he studied distributed systems.  Yaron has lectured, blogged and
written about OCaml for years, with articles published in
Communications of the ACM and the Journal of Functional Programming.
He chairs the steering committee of the Commercial Users of Functional
Programming, and is a member of the steering committee for the
International Conference on Functional Programming.

## Advisers

We will also invite a group of prominent OCaml users to deliver early private
feedback on the book drafts. These include:

- Xavier Leroy (primary architect of OCaml)
- Damien Doligez (primary developer)
- Fabrice Le Fessant (OCamlPro, commercial support)
- Marius Eriksen (Twitter, Inc.)
- David Scott (Citrix Systems)
- Jake Donham (Twitter, Inc.)
- Bryan O'Sullivan (author of Real World Haskell)
- Martin Jambon (Wink)
- Vincent Balat (author of js_of_ocaml and the Ocsigen framework)
- Markus Mottl (author of many critical OCaml libraries)
- Prashanth Mundkur (Nokia, author of Disco big data)
