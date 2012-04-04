# Functors and First-class modules

_(yminsky: Highly preliminary)_

Up until now, modules have played a limited role in our programming.
They are a way of organizing code into units, along with a mechanism
for specifying interfaces for those units.  But that is only part of
what modules do in OCaml.  OCaml's module system is in fact a powerful
toolset for structuring larger-scale systems.  In this chapter, we'll
try to give you a taste of the full power of this system.

## Functors

One fundamental component of OCaml's module system is the _functor_.
A functor is at its core a function from modules to modules.  In order
to get a concrete understanding of what functor are and how they work,
we'll start by playing around with some simple examples.

We'll start with something quite simple indeed: a functor for
incrementing an integer.  Or, more precisely, a functor that takes a
module containing an integer value, and returns a new module,
containing the incremented integer instead.

We'll start by defining a module type for a module that just contains
an integer:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module type M_int = sig val x : int end;;
module type M_int = sig val x : int end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now, we'll ....

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Increment (M:S) : S = struct
    let x = M.x + 1
  end;;
module Increment : functor (M : S) -> sig val x : int end
# module Three = struct let x = 3 end;;
  module Three : sig val x : int end
# module Four = Increment(Three);;
module Four : sig val x : int end
# Four.x - Three.x;;
- : int = 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here's the basic syntax of a functor definition:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
module F (<mod1> : <sig1>) (<mod2> : <sig2>) ... (<modN> : <sigN>) : <sig> = struct
  <implementation>
end

~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here `<mod1>` ...`<modN>` are module names, and `<sig1>` ... `<sigN>`
are module types.  Unlike ordinary functions, with functors, the types
of the arguments can not be inferred.  The final `: <sig>` is a
constraint on the module type of the output of the functor.  The
output type can actually be inferred, so that final signature is
optional.

A functor will accept as input any module that satisfies the
signature, in the same way that a given module implementation in an
`.ml` file can satisfy the signature in its `.mli`.  Thus, the input
signature can drop elements of the module, and can have more abstract
types.  Consider the following example.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Three_and_more = struct
    let x = 3
    let y = "three"
  end;;
module Three_and_more : sig val x : int val x_string : string end
# module Four = Increment(Three_and_more);;
module Four : sig val x : int end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here, we were able to pass in `Three_and_more`, even though
`Three_and_more` had the field `y`, which is not mentioned in the
signature applied to that argument.  




### A worked example

Let's walk through a small, complete example of how to use functors.
Let's go back to frequency count program that we discussed last
chapter.  We experimented with multiple different implementations of
the data-structure for storing the frequency counts.  But what if we
wanted to make the frequency count data-structure pluggable, so we
could instantiate the program with different implementations?
Functors allow us to do just that.

The first step towards making the frequency-count datastructure
pluggable is to specify precisely the interface.  We can do this by
declaring an interface as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* counter_intf.ml *)

open Core.Std

module type S = sig
  type t

  val empty : t
  val to_list : t -> (string * int) list
  val touch : t -> string -> t
end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now, we can write the main program as a functor.

~~~~~~~~~~~~~~~~ { .ocaml }
(* freq.ml: using Counter *)

open Core.Std

module Make(Counter : Counter_intf.S) = struct

let rec build_counts counts =
  match In_channel.input_line stdin with
  | None -> counts
  | Some line -> build_counts (Counter.touch counts line)

let () =
  let counts = build_counts [] in
  let sorted_counts = List.sort counts
    ~cmp:(fun (_,x) (_,y) -> Int.descending x y)
  in
  List.iter (List.take sorted_counts 10)
    ~f:(fun (line,count) -> printf "%3d: %s\n" count line)

end
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Detritus

A functor is essentially a function from modules to modules.  Let's
walk through a small, complete example of how to use functors.  In
this case, we're going to show how you can use a functor build up a
larger module interface from a few key components.  First, let's start
with the interface, in particular, an interface for something that
supports the comparison operator.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
module type Comparable = sig
  type t
  val compare : t -> t -> int
  val (=)  : t -> t -> bool
  val (<>) : t -> t -> bool
  val (>)  : t -> t -> bool
  val (<)  : t -> t -> bool
  val (>=) : t -> t -> bool
  val (<=) : t -> t -> bool
  val min  : t -> t -> t
  val max  : t -> t -> t
end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is in fact a subset of the `Comparable` interface in Core.

Now, suppose we want to implement multiple modules that support this
interface.  Implementing all of these individual functions for each
module is going to be a lot of painful boilerplate, since it's
essentially the same logic that ties it all together.

By using a functor, however, we can derive an implementation that
satisfies this interface provided with just the `compare` function.
In particular, we'll require as input a module that satisfies this
signature:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
module type Comparable_input = sig
  type t
  val compare : t -> t -> int
end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

and we'll create a new module that satisfies the `Comparable`
signature.  Here's a functor which does just that.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
module Make(M:Comparable_input) : Comparable with type t = M.t = struct
  type t = M.t
  let compare = M.compare
  let (=) x y = compare x y = 0
  let (<>) x y = compare x y <> 0
  let (>) x y = compare x y > 0
  let (<) x y = compare x y < 0
  let (>=) x y = compare x y >= 0
  let (<=) x y = compare x y <= 0
  let min x y = if x < y then x else y
  let max x y = if x > y then x else y
end
~~~~~~~~~~~~~~~~~~~~~~~~~~~
