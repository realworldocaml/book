# Fix: memoization and fixed points made easy

`fix` is an OCaml library that helps with various constructions
that involve memoization and recursion.

## Installation

Type `opam install fix`.

## Overview

At the top of an OCaml module, declare `open Fix`.
This gives you access to the following submodules:

* [`DataFlow`](src/DataFlow.ml) performs a forward data flow analysis
  over a directed graph. Like [`Fix`](src/Core.mli), it computes the
  least function of type `variable -> property` that satisfies a fixed
  point equation. It is less widely applicable than `Fix`, but, when
  it is applicable, it is easier to use and more efficient than `Fix`.

* [`Gensym`](src/Gensym.mli) offers a simple facility
  for **generating fresh integer identifiers**.

* [`Memoize`](src/Memoize.mli) offers a number of combinators
  that help **construct possibly recursive memoizing functions**, that
  is, functions that lazily record their input/output graph,
  so as to avoid repeated computation.

* [`Tabulate`](src/Tabulate.mli) offers facilities
  for **tabulating a function**, that is, eagerly evaluating this function
  at every point in its domain, so as to obtain an equivalent
  function that can be queried in constant time.

* [`Numbering`](src/Numbering.mli) offers a facility for
  **assigning a unique number** to each value in a certain finite set
  and translating (both ways) between values and their numbers.

* [`GraphNumbering`](src/GraphNumbering.mli) offers a facility for
  **discovering and numbering the reachable vertices** in a finite directed graph.

* [`HashCons`](src/HashCons.mli) offers support for
  **setting up a hash-consed data type**, that is, a data type whose
  values carry unique integer identifiers.

* [`Fix`](src/Core.mli) offers support for **computing
  the least solution of a set of monotone equations**,
  as described in the unpublished paper
  [Lazy Least Fixed Points in ML](http://cambium.inria.fr/~fpottier/publis/fpottier-fix.pdf).
  In other words, it allows defining a recursive function of
  type `variable -> property`, where
  **cyclic dependencies** between variables are allowed,
  and properties must be equipped with a partial order.
  The function thus obtained performs the fixed point
  computation on demand, in an incremental manner,
  and is memoizing.

* `Prop` defines a few common partial orders, including
  [`Prop.Boolean`](src/Boolean.mli),
  [`Prop.Option`](src/Option.mli),
  [`Prop.Set`](src/Set.mli).

* [`Glue`](src/Glue.mli) contains glue code that helps
  build various implementations of association maps.

The signatures that appear in the above files,
such as `MEMOIZER`, `TABULATOR`, `SOLVER`, and so on,
are defined [here](src/Sigs.ml).

## Demos

A few demos are provided:

* [`brz`](demos/brz) sets up a hash-consed representation of regular
  expressions and shows how to convert a regular expression to a deterministic
  finite-state automaton by Brzozowski's method. This demo exploits almost all
  of the submodules listed above, and is accompanied with
  [a commentary](misc/post.md).

* [`cyk`](demos/cyk) presents a CYK-style parsing algorithm as an instance of
  `Fix`.

* [`cfg`](demos/cfg) uses `Fix` to perform certain static analyses of a
  context-free grammar; this includes computing nullability information and
  FIRST sets.

* [`fib`](demos/fib) defines Fibonacci's function in several different ways
  using the fixed-point combinators offered by `Memoize` and `Fix`.

* [`hco`](demos/hco) sets up simple-minded hash-consed trees
  using `HashCons`.
