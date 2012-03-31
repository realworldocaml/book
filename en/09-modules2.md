# Functors and First-class modules

Up until now, modules have played a limited role, used as the way for
organizing the system into different components with well-defined
interfaces.  This is of course important, but it is only part of what
modules can do.  OCaml's module system is part of a powerful toolset
for organizing and putting together complex functionality.

## Functors

A functor is essentially a function from modules to modules.  Let's
walk through a small, complete example of how to use functors.
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
