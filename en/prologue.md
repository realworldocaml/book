# Prologue

## Why OCaml?

The programming languages that you use affect the code you write.
They influence your software's reliability, security and efficiency,
and how easy it is to read, refactor, and extend.  The languages you
know can also deeply affect how you think about programming and
software design.

But not all ideas about how to design a programming language are
created equal.  Over the last 40 years, a few key language features
have emerged that together form a kind of sweet-spot in language
design.  These features include:

* _Garbage collection_ for automatic memory management, now a feature
  of almost every modern high-level language.
* _First-class functions_ that can be passed around like ordinary
  values, as seen in JavaScript and C#.
* _Static type-checking_ to increase performance and reduce the number
  of runtime errors, as found in Java and C#.
* _Parametric polymorphism_, which enables the construction of
  abstractions that work across different datatypes, similar to
  generics in Java and C# and templates in C++.
* Good support for _immutable programming_, _i.e._, programming
  without making destructive updates to data-structures.  This is
  present in traditional functional languages like Scheme, and is also
  found in distributed big data frameworks like Hadoop.
* _Automatic type inference_ to avoid having to laboriously define the
  type of every single variable in a program and instead have them
  inferred based on how a value is used.  Available in a limited form
  in C# with implicitly typed local variables and in C++11 with its
  `auto` keyword.
* _Algebraic datatypes_ and _pattern matching_ to define and
  manipulate complex data structures.  Available in Scala and F#.

Some of you will know and love all of these features, and for others
they will be largely new, but most of you will have seen _some_ of
them in other languages that you've used.  As we'll demonstrate over
the course of this book, there is something transformative about
having them all together and able to interact in a single language.
Despite their importance, these ideas have made only limited inroads
into mainstream languages and when they do arrive there, like
higher-order functions in C# or parametric polymorphism in Java, it's
typically in a limited and awkward form. The only languages that
completely embody these ideas are _statically-typed functional
programming languages_ like OCaml, F#, Haskell, Scala and Standard ML.

Among this worthy set of languages, OCaml stands apart because it
manages to provide a great deal of power while remaining highly
pragmatic. The compiler has a straightforward compilation strategy
that produces performant code without requiring heavy optimization and
without the complexities of dynamic JIT compilation.  This, along with
OCaml's strict evaluation model, makes runtime behavior easy to
predict.  The garbage collector is _incremental_, letting you avoid
large GC-related pauses, and _precise_, meaning it will collect all
unreferenced data (unlike many reference-counting collectors), and the
runtime is simple and highly portable.

All of this makes OCaml a great choice for programmers who want to
step up to a better programming language, and at the same time get
practical work done.

#### A brief history from the 1960s

OCaml was written in 1996 by Xavier Leroy, Jérôme Vouillon, Damien Doligez and
Didier Rémy at INRIA in France.  It was inspired by a long line of research
into ML starting in the 1960s, and continues to have deep links to the academic
community.

ML was originally the _meta language_ of the LCF proof assistant released by
Robin Milner in 1972 (at Stanford, and later at Cambridge).  ML was turned into
a compiler in order to make it easier to use LCF on different machines, and
gradually turned into a fully fledged system of its own by the 1980s.

The first implementation of Caml appeared in 1987, initially created by
Ascander Saurez and later continued by Pierre Weis and Michel Mauny.  In 1990,
Xavier Leroy and Damien Doligez built a new implementation called Caml Light
that was based on a bytecode interpreter with a fast sequential garbage
collector.  Over the next few years useful libraries appeared, such as Michel
Mauny's syntax manipulation tools, and this helped promote the use of Caml in
education and research teams.

Xavier Leroy continued extending Caml Light with new features, which resulted
in the 1995 release of Caml Special Light.  This improved the executable
efficiency significantly by adding a fast native code compiler that made Caml's
performance competitive with mainstream languages such as C++. A module system
inspired by Standard ML also provided powerful facilities for abstraction and
made larger-scale programs easier to construct.

The modern OCaml emerged in 1996, when a powerful and elegant object system was
implemented by Didier Rémy and Jérôme Vouillon.  This object system was notable
for supporting many common OO idioms in a statically type-safe way, whereas the
same idioms required runtime checks in languages such as C++ or Java.  In 2000,
Jacques Garrigue extended OCaml with several new features such as polymorphic
methods and variants and labeled and optional arguments.

The last decade has seen OCaml attract a significant user base, and language
improvements have been steadily added to support the growing commercial and
academic codebases.  First-class modules, Generalized
Algebraic Data Types (GADTs) and dynamic linking have improved the flexibility
of the language and there is fast native code support for x86_64, ARM, PowerPC,
and Sparc, making OCaml a good choice for systems where resource usage,
predictability, and performance all matter.

### The Core Standard Library

A language on its own isn't enough.  You also need a rich set of
libraries to base your applications on.  A common source of
frustration for those learning OCaml is that the standard library that
ships with the compiler is limited, covering only a small subset of
the functionality you would expect from a general-purpose standard
library.  That's because the standard library isn't a general-purpose
tool; it was developed for use in bootstrapping the compiler, and is
purposefully kept small and simple.

Happily, in the world of open-source software nothing stops
alternative libraries from being written to supplement the
compiler-supplied standard library, and this is exactly what the Core
distribution is.

Jane Street, a company that has been using OCaml for more than a
decade, developed Core for its own internal use, but designed it from
the start with an eye towards being a general-purpose standard
library.  Like the OCaml language itself, Core is engineered with
correctness, reliability, and performance in mind.

Core is distributed with syntax extensions which provide useful new
functionality to OCaml, and there are additional libraries such as the
Async network communications library that extend the reach of Core
into building complex distributed systems.  All of these libraries are
distributed under a liberal Apache 2 license to permit free use in
hobby, academic, and commercial settings.

### The OCaml Platform

Core is a comprehensive and effective standard library, but there's
much more OCaml software out there.  A large community of programmers
have been using OCaml since its first release in 1996 and have
generated many useful libraries and tools.  We'll introduce some of
these libraries in the course of the examples presented in the book.

The installation and management of these third-party libraries is made
much easier via a package management tool known as OPAM.  We'll
explain more about OPAM as the book unfolds, but it forms the basis of
the Platform, which is a set of tools and libraries that, along with
the OCaml compiler, let you build real-world applications quickly and
effectively.

We'll also use OPAM for installing the <command>utop</command> command-line interface.  This is
a modern interactive tool that supports command history, macro expansion,
module completion, and other niceties that make it much more pleasant to work
with the language.  We'll be using <command>utop</command> throughout the book to let you step
through worked examples interactively.

## About this book

Real World OCaml is aimed at programmers who have some experience with
conventional programming languages, but not specifically with
statically-typed functional programming.  Depending on your
background, many of the concepts we cover will be new, including
traditional functional-programming techniques like higher-order
functions and immutable data types, as well as aspects of OCaml's
powerful type and module systems.

If you already know OCaml, this book may surprise you.  Core redefines most of
the standard namespace to make better use of the OCaml module system and expose
a number of powerful, reusable datastructures by default.  Older OCaml code
will still interoperate with Core, but you may need to adapt it for maximal
benefit.  All the _new_ code that we write uses Core, and we believe the Core
model is worth learning; it's been successfully used on large,
multi-million-line codebases and removes a big barrier to building
sophisticated applications in OCaml.

Code that uses only the traditional compiler standard library will always
exist, but there are other online resources to learn how that works.  Real
World OCaml focuses on the techniques the authors have used in their personal
experience to construct scalable, robust software systems.

### What to expect

Real World OCaml is split into three parts:

*    Part I covers the language itself, opening with a guided tour
     designed to provide a quick sketch of the language.  Don't expect to
     understand everything in the tour; it's meant to give you a taste of
     many different aspects of the language, but the ideas covered there
     will be explained in more depth in the chapters that follow.

     After covering the core language, Part I then moves onto more
     advanced features like modules, functors and objects, which may take
     some time to digest.  Understanding these concepts is important, though.
     These ideas will put you in good stead even beyond OCaml when
     switching to other modern languages, many of which have drawn
     inspiration from ML.

*    Part II builds on the basics by working through useful tools and
     techniques for addressing common practical applications, from
     command-line parsing to asynchronous network programming.
     Along the way, you'll see how some of the concepts from Part I are
     glued together into real libraries and tools that combine different
     features of the language to good effect.

*    Part III discusses OCaml's runtime system and compiler toolchain. It
     is remarkably simple when compared to some other language implementations
     (such as Java's or .NET's CLR). 
     Reading this part will enable you to build very high
     performance systems, or to interface with C libraries.  This is also
     where we talk about profiling and debugging techniques using tools
     such as GNU <command>gdb</command>.

### Installation Instructions 

Real World OCaml uses some tools that we've developed while writing this book.
Some of these resulted in improvements to the OCaml compiler, which means that
you will need to ensure that you have an up-to-date development environment
(using the 4.01 version of the compiler).  We've automated everything you need
to do via the OPAM package manager, so please do follow the installation
instructions in [xref](#installation) carefully.

As of publication, the Windows operating system is unsupported by Core, and so
only Mac OS X, Linux, FreeBSD and OpenBSD can be expected to work reliably.
Please check the online installaton instructions for updates regarding Windows,
or install a Linux virtual machine to work through the book as it stands.

This book is not intended as a reference manual.  We aim to teach you about the
language, and about libraries tools and techniques that will help you be a more
effective OCaml programmer.  But it's no replacement for API documentation or
the OCaml manual and man pages.  You can find documentation for all of the
libraries and tools referenced in the book
[online](https://realworldocaml.org/doc).

### Code Examples

All of the code examples in this book are available freely online under a
public-domain-like license.  You are most welcome to copy and use any of the
snippets as you see fit in your own code, without any attribution or other
restrictions on their use.

The code repository is available online at:
<https://github.com/realworldocaml/examples>.  Every code snippet in the book
has a header which tells you the filename in that repository to the find the
source code, shell script, or ancillary data file that the snippet was sourced
from.

## About the Authors

### Yaron Minsky

Yaron Minsky heads the Technology group at Jane Street, a proprietary trading
firm that is the largest industrial user of OCaml.  He was responsible for
introducing OCaml to the company and for managing the company's transition to
using OCaml for all of its core infrastructure.  Today, billions of dollars
worth of securities transactions flow each day through those systems.

Yaron obtained his PhD in Computer Science from Cornell University, where he
studied distributed systems.  Yaron has lectured, blogged and written about
OCaml for years, with articles published in Communications of the ACM and the
Journal of Functional Programming.  He chairs the steering committee of the
Commercial Users of Functional Programming, and is a member of the steering
committee for the International Conference on Functional Programming.

### Anil Madhavapeddy

Anil Madhavapeddy is a Senior Research Fellow at the University of Cambridge,
based in the Systems Research Group. He was on the original team that developed
the Xen hypervisor and helped develop an industry-leading cloud management
toolstack written entirely in OCaml. This XenServer product has been deployed
on millions of physical hosts, and drives critical infrastructure for many
Fortune 500 companies.

Prior to obtaining his PhD in 2006 from the University of Cambridge, Anil had a
diverse background in industry at NetApp, NASA, and Internet Vision.  He is an
active member of the open-source development community with the OpenBSD
operating system, is on the steering committee of the ACM Commercial Uses of
Functional Programming workshop, and serves on the boards of startup companies
where OCaml is extensively used.  He has also developed the Mirage unikernel
system that is written entirely in OCaml from the device drivers up.

### Jason Hickey

Jason Hickey is a Software Engineer at Google Inc. in Mountain View,
California.  He is part of the team that designs and develops the global
computing infrastructure used to support Google services, including the
software systems for managing and scheduling massively distributed computing
resources.

Prior to joining Google, Jason was an Assistant Professor of Computer Science
at Caltech, where he researched reliable and fault-tolerant computing systems,
including programming language design, formal methods, compilers, and new
models of distributed computation.  He obtained his PhD in Computer Science
from Cornell University, where he studied programming languages.  He is the
author of the MetaPRL system, a logical framework for design and analysis of
large software systems; and OMake, an advanced build system for large software
projects.  He is the author of the textbook, _An Introduction to Objective
Caml_ (unpublished).


