# First-class modules

_(highly preliminary)_

OCaml is in some sense broken up into two sub-languages: a value
language and a module language.  The value language is concerned with
so-called "ordinary" values like integers, strings, functions and
algebraic datatypes.  This language is syntactically light-weight, in
part due to type-inference, and supports parametric polymorphism.

The module language is syntactically more heavyweight, in part due to
its lack of type-inference, and it supports a style of polymorphism
that looks more like sub-typing, where a module can be used in a given
context if a subset of its interface matches a given signature.  The
module language is in significant ways more powerful than the value
language, allowing you to create new modules and types using functors.

The extra power of the module system comes at some cost.  Many of the
ordinary things that we are used to from the value language are
unavailable in the module language.  You can't create a list of
modules, or build a module whose implementation is chosen dynamically
at run-time, or pass a module into an ordinary function.

_First class modules_ allow you to bridge the gap between the module
language and the value language, making it possible to package up an
ordinary OCaml module as a value, and then later to unpack that value
as a module.  This lets you manipulate modules using the more flexible
and dynamic value language, which increases the power of the overall
system considerably.

The type of a first class module is tied to a module signature.
Here's a simple example of a module signature for a generic container.
As an example, we'll consider an extension of the `Foldable.S`
interface we discussed in the previous chapter.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module type Foldable = sig
     type 'a t
     val of_list : 'a list -> 'a t
     val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
  end;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can now create a first-class module from this type basing it on any
module that matches this signature

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
module type Matrix = sig
   type t
   val init : int -> int -> float -> t
   val get_exn : t -> int -> int -> float
   val set_exn : t -> int -> int -> float -> unit
   val mult : t -> t -> t
end
~~~~~~~~~~~~~~~~~~~~~~~~~~~






Or, at least, you can't do any of these with
ordinary modules.


## Dynamically choosing a module


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
