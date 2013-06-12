# Prologue

## Why OCaml?

The programming languages that you use affect the software you create.
They influence your software's reliability, security and efficiency,
and how easy it is to read, refactor, and extend.  The languages
you know can also deeply affect how you think about programming and
software design.

But not all ideas about how to design a programming language are
created equal.  Over the last 40 years, a few key language features
have emerged that together form a kind of sweet-spot in language
design.  These features include:

* _Garbage collection_ for automatic memory management, now a feature
  of almost every modern high-level language.
* _Higher-order functions_ that can be passed around as first-class
  values, as seen in Javascript or Scala.
* _Static type-checking_ to reduce runtime errors, such as Java or Scala
  interfaces or variable type declarations in C#, Ada and Pascal.
* _Generics_ to enable abstractions to be constructed across different
  datatypes, available as C++ templates or in Java or C#.
* _Immutable data structures_ that cannot be destructively updated,
  famously enforced in Haskell but also a common feature of many
  distributed big data frameworks.
* _Algebraic datatypes_ and _pattern matching_ to define and 
  manipulate complex data structures, available in Miranda, F# and Standard ML.
* _Automatic type inference_ to avoid having to laboriously define the
  type of every single variable in a program, and instead have them inferred
  based on how a value is used.  Available in Standard ML, F# and even modern
  C++11 via its `auto` keyword.

Some of you will know and love these features, and others will be completely
new to them.  Most of you will have seen _some_ of them in other languages that
you've used.  As we'll demonstrate over the course of this book, it turns out
that there is something transformative about having them all together and able
to interact in a single language.  Despite their importance, these ideas have
made only limited inroads into mainstream languages and when they do arrive
there, like higher-order functions in C# or parametric polymorphism in Java,
it's typically in a limited and awkward form. The only languages that
completely embody these ideas are statically-typed functional programming
languages like OCaml, F#, Haskell, Scala and Standard ML.

Among this worthy set of languages, OCaml stands apart because it manages to
provide a great deal of power while remaining highly pragmatic. The compiler
has a straightforward compilation strategy without excessive optimization
passes, and its strict evaluation model makes runtime behaviour easy to
predict.  The garbage collector is an incremental, precise implementation with
no dynamic JIT compilation, and the runtime is simple and portable across
platforms.

It is all of this that makes OCaml a great choice for programmers who want to
step up to a better programming language, and at the same time want to get
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

In 1990, Xavier Leroy and Damien Doligez built a new implementation called Caml
Light that was based on a bytecode interpreter with a fast sequential garbage
collector.  Over the next few years useful libraries appeared, such as Michel
Mauny's parsing system. Efficiency further improved with a fast native
code compiler that made OCaml's performance competitive with mainstream
languages such as C++.  A module system inspired by Standard ML also provided
powerful facilities for abstraction and larger scale programs.

The modern OCaml emerged in 1996, when a powerful and elegant object system was 
implemented by Didier Rémy and Jérôme Vouillon.  This object system was notable
for supporting many common OO idioms in a statically type-safe way, whereas
the same idioms required runtime checks in languages such as C++ or Java.
In 2000, Jacques Garrique extended OCaml with several new features such as 
polymorphic methods and variants and labelled and optional arguments.

The last decade has seen OCaml attract a significant user base, and language
improvements have been steadily added to support the growing codebases that use
the language both commercially and for academic use.  First-class modules,
Generalized Algebraic Data Types (GADTs) and dynamic linking have improved the
flexibility of the language, and there is fast native code support for x86_64,
ARM, PowerPC and Sparc64, making OCaml a good choice for systems where resource
usage, predictability and performance all matter.  

### The Core Standard Library

A language on its own isn't enough.  You also need a rich set of libraries to
base your applications on.  A common source of frustration for those learning
OCaml is that the standard library that ships with the compiler doesn't provide
a lot of features.  This standard library was actually developed for use within
the compiler itself, and by design covers only a small subset of the
functionality you expect for more general-purpose use.

In the world of open-source software, nothing stops alternative libraries from
being written to supplement the compiler standard library, and this exactly
what the Core distribution is.  Jane Street, a company that has been using
OCaml for more than a decade, developed Core for its own internal use, but it
was designed from the start with an eye towards being a general-purpose
standard library, and has very broad applicability.  Like the OCaml language
itself, Core is also engineered with correctness, reliability and performance
in mind.

Core is distributed with syntax extensions which provide useful new
functionality to OCaml, and there are additional libraries such as the Async
network communications library that extend the reach of Core into building
complex distributed systems.  All of these libraries are distributed under a
liberal Apache 2 license to permit free use in hobby, academic and commercial
settings.

If you've learnt some OCaml before, this book may surprise you with some
differences from your past experience.  Core redefines most of the standard
modules to be much more consistent, and so you'll need to adapt older code.  We
believe the Core model is worth learning; it's been successfully used on large,
million-line codebases  and removes a big barrier to more widespread OCaml
adoption.  There will always exist code that uses only the compiler standard
library of course, but there are other online resources available to learn
that.  Real World OCaml focuses on the techniques the authors have used in
their personal experience to construct scalable, robust computer systems.

### The OCaml Platform

Core is a comprehensive and effective standard library, but there's a lot more
out software out there.  A large community of programmers have been using OCaml
since its first release in 1996 and have generated a lot of useful libraries
and tools.  In Real World OCaml, we'll introduce some of these libraries for
you to experiment with realistic examples.  The installation and management of
these third-party libraries is made much easier via a package management tool
known as OPAM.  We'll explain more about OPAM as the book unfolds, but it forms
the basis of the Platform, which is a set of tools and libraries that, along
with the OCaml compiler, let you build realistic applications quickly and
effectively.

Another big improvement in Core is the `utop` command-line interface.  This is
a modern interactive tool that supports command history, macro expansion,
module completion, and other niceties that make it much more pleasant to work
with the language.  We'll be using `utop` throughout the book instead of the
normal OCaml toplevel.  It can, of course, be installed using OPAM, and
[xref](#installation) guides you through that process.

## About this book

Real World OCaml is aimed at programmers who have some experience with
conventional programming languages, but not specifically with _statically-typed
functional programming_.  The world of dynamic scripting languages such as
Javascript, Ruby and Python have all adopted healthy elements of functional
programming, but not all of it.  Real World OCaml takes you through the full
lifecycle of how to construct software with static typing, including the
powerful module system that makes code reuse so much more robust.

At the same time, OCaml is not Haskell.  It takes a much more pragmatic
approach by being strictly evaluated by default and permitting arbitrary
side-effects.  In fact, you can write OCaml code that looks very similar to
imperative C but remains completely type-safe. One of the major strengths of
OCaml for systems programming is that, with some experience, you can predict
the runtime behaviour of a block of code very easily, with very little compiler
magic involved.  We'll explain some of these tricks to you as we go through the
book and gradually introduce more complex concepts.

### What to expect

Real World OCaml is split into three parts and appendices:

* Part I covers the basic language concepts you'll need to know when building
  OCaml programs.  You won't need to memorise all of this (objects,
  for example, are used rarely in practice), but understanding the
  concepts and examples is important.  This part opens up with a
  guided tour to give you a quick overview of the language.  It then
  moves onto modules, functors and objects, which may take some time
  to digest.  Persevere though; even though these concepts may be
  difficult at first, they will put you in good stead even when
  switching to other languages, many of which have drawn inspiration
  from ML.

* Part II builds on the basics by working through useful tools and techniques.
  Here you'll pick up useful techniques for building networked
  systems, as well as functional design patterns that help combine
  different features of the language to good effect.  The focus
  throughout this section is on networked systems, and among other
  examples we'll build a running example that will perform Internet
  queries using the DuckDuckGo search engine.

* Part III is all about understanding the runtime system in
  OCaml. It's a remarkably simple system in comparison to other
  language runtimes (such as Java or the .NET CLR), and you'll need to
  read this to build very high performance systems that have to
  minimise resource usage or interface to C libraries.  This is also
  where we talk about profiling and debugging techniques using tools
  such as GNU `gdb`.

Contributing your code back to the community is also important (if only to get
bug fixes from other people!), and our appendices explain how to do this via
OPAM and GitHub.

<note>
<title>Note to reviewers</title>

Real World OCaml uses some tools that we've developed while writing this book.
Some of these resulted in improvements to the OCaml compiler, which means that
you will need to ensure that you have an up-to-date development environment
(using the 4.01.0 compiler).  We've automated everything you need via the OPAM
package manager, so please do follow the installation instructions in
[xref](#installation) carefully.

At this stage, the Windows operating system is also unsupported, and only Mac
OS X, Linux, FreeBSD and OpenBSD can be expected to work reliably.  We realize
this is a concern; there are no fundamental barriers to Windows support, but
we're focussed on getting the main content finished before getting stuck into
the porting effort.

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
large software systems; and OMake, an advanced build system for large
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

