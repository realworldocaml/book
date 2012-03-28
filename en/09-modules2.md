# Functors and First-class modules

Up until now, modules have played a limited role, used as the way for
organizing the system into different components with well-defined
interfaces.  This is of course important, but it is only part of what
modules can do.  OCaml's module system is part of a powerful toolset
for organizing and putting together complex functionality.

## Functors

A functor in OCaml is essentially a function from modules to modules.
Let's walk through a small, complete example of how to use functors.
In this case, we're going to show how you can use a functor build up a
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
