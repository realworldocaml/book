# First-class modules

_(highly preliminary)_

Modules as we've seen them so far are a fairly static affair.  While
functors allow you to synthesize new modules, the input of a functor
needs to be specified completely at compile time.  In addition,
modules can't be dealt with like ordinary values, so you can't store
them in a list, or return them from a function.

In this section, we'll show how you can make more dynamic use of
modules bymaking use of so-called _first-class modules_.  As we'll
see, first class modules give us a way of taking a module and
packaging it up as an ordinary value, which can later be unpacked into
an ordinary module.

Let's introduce the mechanics of first-class modules by introducing a
simple application: a logging module with a plug-in architecture based
on first-class modules.

## A logging infrastructure

A good logging library should allow you to log to multiple different
destinations.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* file: logger.mli *)

(** The type of a log *)
val initialize : unit -> unit

val log : string -> unit
~~~~~~~~~~~~~~~~~~~~~~~~~~~




## Detritus

~~~~~~~~~~~~~~~~~~
- Examples
  - Selecting a module at run-time
  - plug-in architecture
  - 
~~~~~~~~~~~~~~~~~~
