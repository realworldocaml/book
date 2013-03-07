# Prologue

## Why OCaml?

The programming languages that you use affect the software you create.
They influence your software's reliability, security and efficiency,
and how easy it is to read, refactor, and extend.  And the languages
you know can also deeply affect how you think about programming and
software design.

But not all ideas about how to design a programming language are
created equal.  Over the last 40 years, a few key language features
have emerged that together form a kind of sweet-spot in language
design.  These features include:

* _Garbage collection_ for automatic memory management, now a feature
  of almost every modern high-level language.
* _Higher-order functions_ that can be passed around as first-class
  values, and seen in Javascript or Scala.
* _Static type-checking_ to reduce run-time errors, such as Java class
  interfaces or Objective-C methods.
* _Generics_ to enable abstractions to be constructed across different
  datatypes, available in Java and .NET.
* _Immutable data structures_ that cannot be destructively updated,
  famously enforced in Haskell but also a common feature of many
  distributed big data frameworks.
* _Algebraic datatypes_ and pattern matching to describe values more
  precisely, available in Miranda, F# and Standard ML.
* _Automatic type inference_ to avoid having to laboriously define the
  type of every single variable in a program and have it inferred
  based on how a value is used.

Some of you will know and love these features, and others will be
completely new to them.  Most of you will have seen _some_ of them in
other languages that you've used.  As we'll demonstrate over the
course of this book, it turns out that there is something
transformative about having them all together and able to interact in
a single language.  Despite their importance, these ideas have made
only limited inroads into mainstream languages. And when they do
arrive there, like higher-order functions in C# or parametric
polymorphism in Java, it's typically in a limited and awkward form.
The only languages that support these ideas well are statically-typed
functional programming languages like OCaml, F#, Haskell, Scala and
Standard-ML.

Among this worthy set of languages, OCaml stands apart because it
manages to provide a great deal of power while remaining highly
pragmatic, highly performant, and comparatively simple to use and
understand.  It is this that makes OCaml a great choice for
programmers who want to step up to a better programming language, and
at the same time want to get practical work done.

### The Core Standard Library

A language on its own isn't enough.  You also need a rich set of
libraries to base your applications on.  A common source of
frustration for those learning OCaml is that the standard library that
ships with the OCaml compiler is not ideal.  While it's well
implemented, it is really intended for use within the compiler itself,
and covers only a small subset of the functionality you expect for
more general-purpose use.

But all is not lost!  There is an effective alternative to the OCaml
standard library called Core.  Jane Street, a company that has been
using OCaml for more than a decade, developed Core for its own
internal use, but it was designed from the start with an eye towards
being a general-purpose standard library.  Core is also distributed
with syntax extensions which provide essential new functionality to
OCaml; and there are additional libraries such as the `Async` network
communications library that provide even more useful functionality.

### The OCaml Platform

Core is a very comprehensive standard library, but there's also a
large community of programmers who have used OCaml since its first
release in 1996.  In Real World OCaml, we'll also introduce some of
these libraries for you to experiment with realistic examples.  The
installation and management of these third-party libraries is made
much easier via a package management tool known as OPAM.  We'll
explain more about OPAM as the book unfolds, but it forms the basis of
the Platform, which is a set of tools and libraries that, along with
the OCaml compiler, let you build realistic applications quickly and
effectively.

Another big improvement over the standard library is the `utop`
interactive top level.  This is a modern interactive tool that
supports command history, macro expansion, module completion, and
other niceties that make it much more pleasant to work with the
language.  We'll be using `utop` throughout the book instead of the
normal OCaml toplevel.  It can, of course, be installed using OPAM,
and [xref](#installation) guides you through that process.

## About this book

Real World OCaml is aimed at programmers who have some experience with
conventional programming languages, but not specifically with
_statically typed functional programming_.  The world of dynamic
scripting languages such as Javascript, Ruby and Python have all
adopted healthy elements of functional programming, but not all of it.
Real World OCaml takes you through the full lifecycle of how to
construct software with static typing, including the powerful module
system that makes code re-use so much more robust.

At the same time, OCaml is not Haskell.  It takes a much more
pragmatic approach by being strictly evaluated by default, and
permitting arbitrary side-effects.  In fact, you can write OCaml code
that looks very similar to C, but is still type-safe. One of the major
strengths of OCaml for systems programming is that, with some
experience, you can predict the runtime behaviour of a block of code
very easily, with very little compiler magic involved.

If you've learnt some OCaml before, this book may surprise you with
some differences from your past experience.  The Core standard library
redefines most of the standard modules to be much more consistent, and
so you'll need to adapt older code.  We believe the Core model is
worth learning; it's been successfully used on large million line
codebases, and removes a big barrier to more widespread OCaml
adoption.  There will always exist code that uses only the compiler
standard library of course, but there are other online resources
available to learn that.  Real World OCaml focuses on the techniques
the authors have used in their personal experience to construct
scalable, robust computer systems.

## What to expect

Real World OCaml is split into three parts and appendices:

* Part I covers the basic concepts you'll need to know when building
  OCaml programs.  You won't need to memorise all of this (objects,
  for example, are used rarely in practice), but understanding the
  concepts and examples is important.  This part opens up with a
  guided tour to give you a quick overview of the language.  It then
  moves onto modules, functors and objects, which may take some time
  to digest.  Persevere though; even though these concepts may be
  difficult at first, they will put you in good stead even when
  switching to other languages, many of which have drawn inspiration
  from ML.

* Part II builds on the basics to construct more complete examples.
  This is where you'll pick up some useful techniques for building
  networked systems, as well as some functional design patterns that
  glue together OCaml language elements in useful ways.  The theme
  throughout this chapter is on networked systems, and we build a
  running example that will perform Internet queries using the
  DuckDuckGo search engine.

* Part III is all about understanding the runtime system in
  OCaml. It's a remarkably simple system in comparison to other
  language runtimes (such as Java or the .NET CLR), and you'll need to
  read this to build very high performance systems that have to
  minimise resource usage or interface to C libraries.  This is also
  where we talk about profiling and debugging techniques using tools
  such as GNU `gdb` and `gprof`.  Contributing your code back to the
  community is also important (if only to get bug fixes from other
  people!), and this part also explains how to do this via OPAM and
  Github.

<note>
<title>Note to reviewers</title>

Real World OCaml uses some tools that we've developed while writing
this book.  Some of these resulted in improvements to the OCaml
compiler, which means that you will need to ensure that you have an
up-to-date development environment (using the 4.01.0 compiler).  We've
automated everything you need via the OPAM package manager, so please
do follow the installation instructions in [xref](#installation)
carefully.

At this stage, the Windows operating system is also unsupported, and
only MacOS X, Linux, FreeBSD and OpenBSD can be expected to work
reliably.  We realize this is a concern; there are no fundamental
barriers to Windows support, but we're focussed on getting the main
content finished before getting stuck into the porting effort.

</note>

## About the Authors

### Jason Hickey

Jason Hickey is a Software Engineer at Google Inc. in Mountain
View, California.  He is part of the team that designs and develops
the global computing infrastructure used to support Google services,
including the software systems for managing and scheduling massively
distributed computing resources.

Prior to joining Google, Jason was an Assistant Professor of Computer
Science at Caltech, where his research was in reliable and
fault-tolerant computing systems, including programming language
design, formal methods, compilers, and new models of distributed
computation.  He obtained his PhD in Computer Science from Cornell
University, where he studied programming languages.  He is the author
of the MetaPRL system, a logical framework for design and analysis of
large software systems; OMake, an advanced build system for large
software projects.  He is the author of the textbook, _An Introduction
to Objective Caml_ (unpublished).

### Anil Madhavapeddy

Anil Madhavapeddy is a Senior Research Fellow at the University of
Cambridge, based in the Systems Research Group. He was on the original
team that developed the Xen hypervisor, and helped develop an
industry-leading cloud management toolstack written entirely in
OCaml. This XenServer product has been deployed on millions of
physical hosts, and drives critical infrastructure for many Fortune
500 companies.

Prior to obtaining his PhD in 2006 from the University of Cambridge,
Anil had a diverse background in industry at NetApp, NASA and Internet
Vision.  He is an active member of the open-source development
community with the OpenBSD operating system, is on the steering
committee of the Commercial Uses of Functional Programming ACM
workshop, and serves on the boards of startup companies where OCaml is
extensively used.  He has also developed the Mirage unikernel system
that is written entirely in OCaml from the device drivers up.

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

