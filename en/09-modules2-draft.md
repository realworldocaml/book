# Functors and First-class modules

_(yminsky: Highly preliminary)_

Up until now, we've seen modules play only a limited role as a way of
organizing code into units, with a mechanism for specifying interfaces
for those units.  But that is only part of what modules can do in
OCaml.  The module system is in fact a powerful toolset for
structuring larger-scale systems.  In this chapter, we'll try to give
you a taste of the power of that system and show ways of using it
effectively.

## Functors

A functor is, roughly speaking, a function from modules to modules.
Functors can be used to solve a variety of code-structuring problems,
including:

* _Dependency injection_, or making the implementations of some
  components of a system swappable.  Among other reasons, this is
  useful to mock up parts of your system for testing and simulation
  purposes.
* _Auto-extension of modules_.  Sometimes, there is a set of
  functionality that you want to build in a standard way based on some
  small piece of type-specific logic.  For example, you might want to
  add a slew of comparison operators derived from a base comparison
  function.  To do this by hand would require a great deal of
  boilerplate, but functors let you do this with a minimum of fuss.
* _Instantiating modules with state_.  Modules can contain mutable
  state, and that means that you'll occasionally want to have multiple
  instantiations of a particular module, each with its own independent
  state.  One simple but common example is that of a unique-id
  allocator.  Functors let you automate the construction of such
  modules.

_(yminsky: awk)_

And that is just part of what functors are good for.

In the following, we'll walk through the details of how functors work,
after which we'll look at examples of how to use functors effectively
in your software designs.

### The basics of functors

In order to get a concrete understanding of what functors are and how
they work, we'll start by playing around with some simple examples,
starting with a very simple example indeed: a functor for incrementing
an integer.

More precisely, we'll create a functor that takes a module containing
a single integer variable `x`, and returns a new module with `x`
incremented by one.  The first step is to define a module type to
describe the input and output of the functor.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module type X_int = sig val x : int end;;
module type X_int = sig val x : int end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now, we can use that module type to write the increment functor.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Increment (M:X_int) : X_int = struct
    let x = M.x + 1
  end;;
module Increment : functor (M : X_int) -> X_int
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Unlike ordinary functions, functors require more explicit type
annotations.  In particular, the module type on input module of a
functor is mandatory.  The module type on the output is not, however,
and would be inferred if it were omitted.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Increment (M:X_int) = struct
    let x = M.x + 1
  end;;
module Increment : functor (M : X_int) -> sig val x : int end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the above, we can see that the module type of the output is written
out explicitly, rather than being a reference to the named signature
`X_int`.

Here's what `Increment` looks like in action.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Three = struct let x = 3 end;;
  module Three : sig val x : int end
# module Four = Increment(Three);;
module Four : sig val x : int end
# Four.x - Three.x;;
- : int = 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this case, we applied `Increment` to a module whose signature is
exactly equal to `X_int`.  But we can apply `Increment` to any module
that satisfies `X_int`, in the same way that an `.ml` file satisfies
the signature in its `.mli`.  So, for example, `Increment` can take as
its input a module that has more fields than are contemplated in
`X_int`.  For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Three_and_more = struct
    let x = 3
    let y = "three"
  end;;
module Three_and_more : sig val x : int val x_string : string end
# module Four = Increment(Three_and_more);;
module Four : sig val x : int end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We were able to pass `Three_and_more` to `Increment`, even though
`Three_and_more` has the field `y` which is not present in `X_int`.

### Working with types

The `Increment` example was especially trivial, in that the modules
that were being operated on contained only values.  But functors can
operate on the types of a module as well.  Imagine, for example, that
we want to create a module for dealing with closed intervals over any
given type.  A functor is one effective way of doing this.

The firs thing we'll need is a module type to represent the type of
values along with a comparison function.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module type Comparable = sig
    type t
    val compare : t -> t -> int
  end ;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the comparison function returns an integer.  The comparison
function should return `0` if the two elements are zero, a positive
number if the first element is larger than the second, and a negative
number if the first element is smaller than the second.  Thus, this is
how we would rewrite the standard comparision functions on top of compare:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
compare x y < 0     (* x < y *)
compare x y = 0     (* x = y *)
compare x y > 0     (* x > y *)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now, we can use this signature as the basis of our implementation.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Make(C : Comparable) = struct
    type t = | Interval of C.t * C.t
             | Empty

    let create low high =
      if C.compare low high > 0 then Empty
      else Interval (low,high)

    let contains t x =
      match t with
      | Empty -> false
      | Interval (l,h) ->
        C.compare x l >= 0 && C.compare x h <= 0

    let intersect t1 t2 =
      let min x y = if C.compare x y <= 0 then x else y in
      let max x y = if C.compare x y >= 0 then x else y in
      match t1,t2 with
      | Empty, _ | _, Empty -> Empty
      | Interval (l1,h1), Interval (l2,h2) ->
        create (max l1 l2) (min h1 h2)

  end ;;
module Make :
  functor (C : Comparable) ->
    sig
      type t = Interval of C.t * C.t | Empty
      val create : C.t -> C.t -> t
      val contains : t -> C.t -> bool
      val intersect : t -> t -> t
    end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can then take this functor, and apply it to concrete modules like
`Int` or `String`.




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
