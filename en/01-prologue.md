# Prologue

_(yminsky: this is something of a placeholder.  We need a real
introduction that should talk, amongst other things, about what kinds
of applications OCaml is good for and why one should want to learn it.
Also, some coverage of who uses OCaml successfully now.)_

## Why OCaml?

Programming languages matter.

The programming languages that you use affect your productivity.  They
affect how reliable your software is, how efficient it is, how easy it
is to read, to refactor, and to extend.  And the programming languages
you know and use can deeply affect how you think about programming and
software design.

But not all ideas about how to design a programming language are
created equal.  Over the last 40 years, a few key language features
have emerged that together form a kind of sweet-spot in language
design.  These features include:

- Garbage collection
- First-class and higher-order functions
- Static type-checking
- Parametric polymorphism
- Support for programming with immutable values
- Algebraic datatypes and pattern-matching
- Type inference

Some of these features you already know and love, and some are
probably new to you.  But as we hope to demonstrate over the course of
this book, it turns out that there is something transformative about
having them all together and able to interact with each other in a
single language.

Despite their importance, these ideas have made only limited inroads
into mainstream languages. And when they do arrive there, like
higher-order functions in C# or parametric polymorphism in Java, it's
typically in a limited and awkward form.  The only languages that
support these ideas well are statically-typed functional programming
languages like OCaml, F#, Haskell, Scala and Standard-ML.

Among this worthy set of languages, OCaml stands apart because it
manages to provide a great deal of power while remaining highly
pragmatic, highly performant, and comparatively simple to use and
understand.  It is this that makes OCaml a great choice for
programmers who want to step up to a better programming language, and
at the same time want to get practical work done.

## Why Core?

A language on its own isn't enough.  You also need a rich set of
libraries to base your applications on.  A common source of
frustration for those learning OCaml is that the standard library that
ships with the OCaml compiler is not ideal.  While it's well
implemented, it covers only a small subset of the functionality you
expect from a standard library, and the interfaces are idiosyncratic
and inconsistent.

But all is not lost!  There is an effective alternative to the OCaml
standard library called Core.  Jane Street, a company that has been
using OCaml for nearly a decade, developed Core for its own internal
use, but it was designed from the start with an eye towards being a
general-purpose standard library.  Core is also distributed with
syntax-extensions which provide essential new functionality to OCaml;
and there are additional libraries, like Core_extended and Async, that
provide even more useful functionality.

We believe that Core makes OCaml a better tool, and that's why we'll
present OCaml and Core together.

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
Cambridge, based in the Systems Research Group. He was on the original team
that developed the Xen hypervisor, and helped develop an industry-leading cloud
management toolstack written entirely in OCaml. This XenServer product has been
deployed on hundreds of thousands of physical hosts, and drives critical
infrastructure for many Fortune 500 companies.

Prior to obtaining his PhD in 2006 from the University of Cambridge, Anil had a
diverse background in industry at Network Appliance, NASA and Internet Vision.
In addition to professional and academic activities, he is an active member of
the open-source development community with the OpenBSD operating system, is
co-chair of the Commercial Uses of Functional Programming workshop, and serves
on the boards of startup companies such as Ashima Arts where OCaml is
extensively used.

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

