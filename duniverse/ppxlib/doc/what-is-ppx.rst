************
What is PPX?
************

Overview
--------

Ppx is a meta-programming system for the OCaml programming
language. It allows developers to generate code at compile time in a
principled way. The distinguishing feature of ppx is that it is
tightly integrated with the OCaml parser and instead of operating at
the text level it operates on the internal structured representation
of the language in the compiler, called the Abstract Syntax Tree or
AST for short.

A few years ago, the OCaml language was extended with two new
constructions: annotations and extension points. Annotations are
arbitrary pieces of information that can be attached to most parts of
the OCaml language. They can be used to control the behavior of the
OCaml compiler, or in some specific cases to generate code at compile
time.

Extension points are compile time functions. The compiler itself
doesn't know how to interpret them and they must all be rewritten by
the ppx system before the compiler can process input files further.

Ppxlib mainly supports two ways of generating code at compile time: by
expanding an extension point or by expanding a ``[@@deriving ...]``
attribute after a type declaration.

How does it works?
------------------

The ppx system is composed of 3 parts:

- individual ppx rewriters
- ppxlib
- a hook in the compiler

Inidividual ppx rewriters are those implemented by various developers
to provide features to end users, such as ppx_expect_ which provides a
good inline testing framework.

All these rewriters are written against the ppxlib API. Ppxlib is
responsible for acknowledging the various rewriters a end user wants
to use, making sure they can be composed together and performing the
actual rewriting of input files.

The hook in the compiler allows ppxlib to insert itself in the
compilation pipeline and perform the rewriting of input files based on
a list of ppx rewriters specified by the user. The hooks take the form
of command line flags that takes a command to execute. The compiler
supports two slightly different flags, for providing commands that are
executed at different stages: ``-pp`` and ``-ppx``. The difference
between the two is as follow:

- ``-pp`` takes as argument a command that is used to parse the
  textual representation. Such a command can produce either a plain
  OCaml source file or a serialised representation of the AST

- ``-ppx`` takes as argument a command that is given a serialised
  representation of the AST and produces another serialised AST

Ppxlib generally uses the first one as it yields faster compilation
times, however it supports both methods of operation.

Is ppxlib necessary?
--------------------

Yes. While authors of ppx rewriters may in theory use the compiler
hooks directly, doing so is strongly discouraged for the following
reasons:

- composing such ppx rewriters is slow and yields much slower
  compilation times
- the ABI of the hook is not stable and regularly changes in
  incompatible ways. This means that a ppx rewriter using the compiler
  hook directly is likely to work only with a single version of the
  OCaml compiler
- the compiler does not provide a good composition semantics, which
  means that input files will not always be transformed as
  expected. It is hard to predict what the final result will be, and
  for end users it is hard to understand what is happening when things
  go wrong
- the compiler doesn't handle hygiene: if an attribute is mistyped or
  misplaced, it is silently ignored by the compiler. If two ppx
  rewriters want to interpret the same attribute or extension point in
  incompatible ways, the result is not specified

In summary, ppxlib abstracts away from all the low-level details of
the ppx system and exposes a consistent model to authors of ppx
rewriters and end users.

Current state of the ppx ecosystem
----------------------------------

Ppxlib was developed after the introduction of the ppx system. As a
result, many ppx rewriters do not currently use ppxlib and are using
the compiler hooks directly. Ppxlib can acknowledge such rewriters so
that they can be used in conjunction with more modern rewriters,
however it cannot provide a good composition or hygiene story when
using such ppx rewriters.

Note on stability regarding new compiler releases
-------------------------------------------------

Due to the nature of the ppx system, it is hard for ppxlib to provide
full protection against compiler changes. This means that a ppx
rewriter written against ppxlib today can be broken by a future
release of the OCaml compiler and a new release of the ppx rewriter
will be necessary to support the new compiler.

However the following is true: every time this might happen, it will be
possible to extend ppxlib to provide a greater protection, so that
eventually the whole ppx ecosystem is completely shielded from
breaking compiler changes.

.. _ppx_expect: https://github.com/janestreet/ppx_expect

