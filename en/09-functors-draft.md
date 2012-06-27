# Functors and First-Class Modules

Up until now, we've seen modules play a limited role, serving as a
mechanism for organizing code into units with specified interfaces.
But OCaml's module system plays a bigger role in the langauge, acting
as a powerful toolset for structuring large-scale systems.  This
chapter will introduce you to functors and first class modules, which
greatly increase the power of the module system.

## Functors

Functors are, roughly speaking, functions from modules to modules, and
they can be used to solve a variety of code-structuring problems,
including:

* _Dependency injection_, or making the implementations of some
  components of a system swappable.  This is particularly useful when
  you want to mock up parts of your system for testing and simulation
  purposes.
* _Auto-extension of modules_.  Sometimes, there is some functionality
  that you want to build in a standard way for different types, in
  each case based on a some piece of type-specific logic.  For
  example, you might want to add a slew of comparison operators
  derived from a base comparison function.  To do this by hand would
  require a lot of repetitive code for each type, but functors let you
  write this logic just once and apply it to many different types.
* _Instantiating modules with state_.  Modules can contain mutable
  state, and that means that you'll occasionally want to have multiple
  instantiations of a particular module, each with its own separate
  and independent mutable state.  Functors let you automate the
  construction of such modules.

### A trivial example

We'll start by considering the simplest possible example: a functor
for incrementing an integer.

More precisely, we'll create a functor that takes a module containing
a single integer variable `x`, and returns a new module with `x`
incremented by one.  The first step is to define a module type which
will describe the input and output of the functor.

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

One thing that immediately jumps out about functors is that they're
considerably more heavyweight syntactically than ordinary functions.
For one thing, functors require explicit type annotations, which
ordinary functions do not.  Here, we've specified the module type for
both the input and output of the functor.  Technically, only the type
on the input is mandatory, although in practice, one often specifies
both.

The following shows what happens when we omit the module type for the
output of the functor.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Increment (M:X_int) = struct
    let x = M.x + 1
  end;;
module Increment : functor (M : X_int) -> sig val x : int end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can see that the inferred module type of the output is now written
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
that satisfies `X_int`.  So, for example, `Increment` can take as its
input a module that has more fields than are contemplated in `X_int`,
as shown below.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Three_and_more = struct
    let x = 3
    let y = "three"
  end;;
module Three_and_more : sig val x : int val x_string : string end
# module Four = Increment(Three_and_more);;
module Four : sig val x : int end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

### A bigger example: computing with intervals

We'll now look at a more complex example, which will give us an
opportunity to learn more about how functors work.  In particular,
we'll walk through the design of a library for computing with
intervals.  This library will be functorized over the type of the
endpoints of the intervals and the ordering of those endpoints.

First we'll define a module type that captures the information we'll
need about the endpoint type.  This interface, which we'll call
`Comparable`, contains just two things: a comparison function, and the
type of the values to be compared.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module type Comparable = sig
    type t
    val compare : t -> t -> int
  end ;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The comparison function follows the standard OCaml idiom for such
functions, returning `0` if the two elements are equal, a positive
number if the first element is larger than the second, and a negative
number if the first element is smaller than the second.  Thus, we
could rewrite the standard comparison functions on top of `compare` as
shown below.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
compare x y < 0     (* x < y *)
compare x y = 0     (* x = y *)
compare x y > 0     (* x > y *)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now that we have the `Comparable` interface, we can write the
implementation of our interval module.  In this module, we'll
represent an interval with a variant type, which is either `Empty` or
`Interval (x,y)`, where `x` and `y` are the bounds of the interval.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Make_interval(Endpoint : Comparable) = struct

    type t = | Interval of Endpoint.t * Endpoint.t
             | Empty

    let create low high =
      if Endpoint.compare low high > 0 then Empty
      else Interval (low,high)

    let is_empty = function
      | Empty -> true
      | Interval _ -> false

    let contains t x =
      match t with
      | Empty -> false
      | Interval (l,h) ->
        Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

    let intersect t1 t2 =
      let min x y = if Endpoint.compare x y <= 0 then x else y in
      let max x y = if Endpoint.compare x y >= 0 then x else y in
      match t1,t2 with
      | Empty, _ | _, Empty -> Empty
      | Interval (l1,h1), Interval (l2,h2) ->
        create (max l1 l2) (min h1 h2)

  end ;;
module Make_interval :
  functor (Endpoint : Comparable) ->
    sig
      type t = Interval of Endpoint.t * Endpoint.t | Empty
      val create : Endpoint.t -> Endpoint.t -> t
      val contains : t -> Endpoint.t -> bool
      val intersect : t -> t -> t
    end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can instantiate the functor by applying it to a module with the
right signature.  In the following, we provide the functor input as an
anonymous module.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# module Int_interval =
    Make_interval(struct
      type t = int
      let compare = Int.compare
    end);;
module Int_interval :
  sig
    type t = Interval of int * int | Empty
    val create : int -> int -> t
    val contains : t -> int -> bool
    val intersect : t -> t -> t
  end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we choose our interfaces to be aligned with the standards of our
libraries, then we often don't have to construct a custom module for a
given functor.  In this case, for example, we can directly use the
`Int` or `String` modules provided by Core.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Int_interval = Make_interval(Int) ;;
# module String_interval = Make_interval(String) ;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This works because many modules in Core, including `Int` and `String`,
satisfy an extended version of the `Comparable` signature described
above.  As a general matter, having standardized signatures is a good
practice, both because a more uniform codebase is easier to navigatge,
and because it makes functors easier to use.

Now we can use the newly defined `Int_interval` module like any
ordinary module.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let i1 = Int_interval.create 3 8;;
val i1 : Int_interval.t = Int_interval.Interval (3, 8)
# let i2 = Int_interval.create 4 10;;
val i2 : Int_interval.t = Int_interval.Interval (4, 10)
# Int_interval.intersect i1 i2;;
- : Int_interval.t = Int_interval.Interval (4, 8)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This design gives us the freedom to use any comparison function we
want for comparing the endpoints.  We could, for example, create a
type of int interval with the order of the comparison reversed, as
follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Rev_int_interval =
    Make_interval(struct
      type t = int
      let compare x y = Int.compare y x
    end);;
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The behavior of `Rev_int_interval` is of course different from
`Int_interval`, as we can see below.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let interval = Int_interval.create 4 3;;
val interval : Int_interval.t = Int_interval.Empty
# let rev_interval = Rev_int_interval.create 4 3;;
val rev_interval : Rev_int_interval.t = Rev_int_interval.Interval (4, 3)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Importantly, `Rev_int_interval.t` is a different type than
`Int_interval.t`, even though its physical represenation is the same.
Indeed, the type system will prevent us from confusing them.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Int_interval.contains rev_interval 3;;
Characters 22-34:
  Int_interval.contains rev_interval 3;;
                        ^^^^^^^^^^^^
Error: This expression has type Rev_int_interval.t
       but an expression was expected of type
         Int_interval.t = Make_interval(Int).t
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is important, because confusing the two kinds of intervals would
be a semantic error, and it's an easy one to make.  The ability of
functors to mint new types is a useful trick that comes up a lot.

#### Making the functor abstract

There's a problem with `Make_interval`.  The code we wrote depends on
the invariant that the upper bound of an interval is greater than its
lower bound, but that invariant can be violated.  The invariant is
enforced by the create function, but because `Interval.t` is not
abstract, we can bypass the `create` function.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Int_interval.create 4 3;; (* going through create *)
- : Int_interval.t = Int_interval.Empty
# Int_interval.Interval (4,3);; (* bypassing create *)
- : Int_interval.t = Int_interval.Interval (4, 3)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

To make `Int_interval.t` abstract, we need to apply an interface to
the output of the `Make_interval`.  Here's an explicit interface that
we can use for that purpose.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module type Interval_intf = sig
   type t
   type endpoint
   val create : endpoint -> endpoint -> t
   val is_empty : t -> bool
   val contains : t -> endpoint -> bool
   val intersect : t -> t -> t
  end;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This interface includes the type `endpoint` to represent the type of
the endpoints of the interval.  Given this interface, we can redo our
definition of `Make_interval`, as follows.  Notice that we added the
type `endpoint` to the implementation of the module to make the
implementation match `Interval_intf`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Make_interval(Endpoint : Comparable) : Interval_intf = struct

    type endpoint = Endpoint.t
    type t = | Interval of Endpoint.t * Endpoint.t
             | Empty

    ....

  end ;;
module Make_interval : functor (Endpoint : Comparable) -> Interval_intf
~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Sharing constraints

The resulting module is abstract, but unfortunately, it's too
abstract.  In particular, we haven't exposed the type `endpoint`,
which means that we can't even construct an interval anymore.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Int_interval = Make_interval(Int);;
module Int_interval : Interval_intf
# Int_interval.create 3 4;;
Characters 20-21:
  Int_interval.create 3 4;;
                      ^
Error: This expression has type int but an expression was expected of type
         Int_interval.endpoint
~~~~~~~~~~~~~~~~~~~~~~~~~~~

To fix this, we need to expose the fact that `endpoint` is equal to
`Int.t` (or more generally, `Endpoint.t`, where `Endpoint` is the
argument to the functor).  One way of doing this is through a _sharing
constraint_, which allows you to tell the compiler to expose the fact
that a given type is equal to some other type.  The syntax for a
sharing constraint on a module type is as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
S with type t = s
~~~~~~~~~~~~~~~~~~~~~~~~~~~

where `S` is a module type, `t` is a type inside of `S`, and `s` is a
different type.  The result of this expression is a new signature
that's been modified so that it exposes the fact that `t` is equal to
`s`.  We can use a sharing constraint to create a specialized version
of `Interval_intf` for integer intervals.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module type Int_interval_intf = Interval_intf with type endpoint = int;;
module type Int_interval_intf =
  sig
    type t
    type endpoint = int
    val create : endpoint -> endpoint -> t
    val is_empty : t -> bool
    val contains : t -> endpoint -> bool
    val intersect : t -> t -> t
  end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

And we can also use it in the context of a functor, where the
right-hand side of the sharing constraint is an element of the functor
argument.  Thus, we expose an equality between a type in the output of
the functor (in this case, the type `endpoint`) and a type in its
input (`Endpoint.t`).

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Make_interval(Endpoint : Comparable)
      : Interval_intf with type endpoint = Endpoint.t = struct

    type endpoint = Endpoint.t
    type t = | Interval of Endpoint.t * Endpoint.t
             | Empty

    ...

  end ;;
module Make_interval :
  functor (Endpoint : Comparable) ->
    sig
      type t
      type endpoint = Endpoint.t
      val create : endpoint -> endpoint -> t
      val is_empty : t -> bool
      val contains : t -> endpoint -> bool
      val intersect : t -> t -> t
    end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

So now, the interface is as it was, except that `endpoint` is now
known to be equal to `Endpoint.t`.  As a result of that type equality,
we can now do things like construct intervals again.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let i = Int_interval.create 3 4;;
val i : Int_interval.t = <abstr>
# Int_interval.contains i 5;;
- : bool = false
~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Destructive substitution

Sharing constraints basically do the job, but the approach we used has
some downsides.  In particular, we've now been stuck with the useless
type declaration of `endpoint` that clutters up both the interface and
the implementation.  A better solution would be to modify the
`Interval_intf` signature by replacing `endpoint` with `Endpoint.t`
everywhere it shows up, making `endpoint` unnecessary.  We can do just
this using what's called _destructive substitution_.  Here's the basic
syntax.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
S with type t := s
~~~~~~~~~~~~~~~~~~~~~~~~~~~

where `S` is a signature, `t` is a type inside of `S`, and `s` is a
different type.  The following shows how we could use this with
`Make_interval`.

Here's an example of what we get if we use destructive substitution to
specialize the `Interval_intf` interface to integer intervals.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module type Int_interval_intf = Interval_intf with type endpoint := int;;
module type Int_interval_intf =
  sig
    type t
    val create : int -> int -> t
    val is_empty : t -> bool
    val contains : t -> int -> bool
    val intersect : t -> t -> t
  end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

There's now no mention of n `endpoint`, all occurrences of that type
having been replaced by `int`.  As with sharing constraints, we can
also use this in the context of a functor.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Make_interval(Endpoint : Comparable)
    : Interval_intf with type endpoint := Endpoint.t =
  struct

    type t = | Interval of Endpoint.t * Endpoint.t
             | Empty

    ....

  end ;;
module Make_interval :
  functor (Endpoint : Comparable) ->
    sig
      type t
      val create : Endpoint.t -> Endpoint.t -> t
      val is_empty : t -> bool
      val contains : t -> Endpoint.t -> bool
      val intersect : t -> t -> t
    end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The interface is precisely what we want, and we didn't need to define
the `endpoint` type alias in the body of the module.  If we
instantiate this module, we'll see that it works properly: we can
construct new intervals, but `t` is abstract, and so we can't directly
access the constructors and vioalte the invariants of the data
structure.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Int_interval = Make_interval(Int);;
# Int_interval.create 3 4;;
- : Int_interval.t = <abstr>
# Int_interval.Interval (4,3);;
Characters 0-27:
  Int_interval.Interval (4,3);;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unbound constructor Int_interval.Interval
~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Using multiple interfaces

Another feature that we might want for our interval module is the
ability to serialize the type, in particular, by converting to
s-expressions.  If we simply invoke the `sexplib` macros by adding
`with sexp` to the definition of `t`, though, we'll get an error:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Make_interval(Endpoint : Comparable)
    : Interval_intf with type endpoint := Endpoint.t = struct

    type t = | Interval of Endpoint.t * Endpoint.t
             | Empty
    with sexp

    ....

  end ;;
Characters 120-123:
        type t = | Interval of Endpoint.t * Endpoint.t
                               ^^^^^^^^^^
Error: Unbound value Endpoint.t_of_sexp
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The problem is that `with sexp` adds code for defining the
s-expression converters, and that code assumes that `Endpoint` has the
appropriate sexp-conversion functions for `Endpoint.t`.  But all we
know about `Endpoint` is that it satisfies the `Comparable` interface, which
doesn't say anything about s-expressions.

Happily, Core comes with a built in interface for just this purpose
called `Sexpable`, which is defined as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
module type Sexpable = sig
  type t = int
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can modify `Make_interval` to use the `Sexpable` interface, for
both its input and its output.  Note the use of destructive
substitution to combine multiple signatures together.  This is
important because it stops the `type t`'s from the different
signatures from interfering with each other.

Also note that we have been careful to override the sexp-converter
here to ensure that the datastructures invariants are still maintained
when reading in from an s-expression.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module type Interval_intf_with_sexp = sig
   type t
   include Interval_intf with type t := t
   include Sexpable      with type t := t
  end;;
# module Make_interval(Endpoint : sig
    type t
    include Comparable with type t := t
    include Sexpable   with type t := t
  end) : Interval_intf_with_sexp with type endpoint := Endpoint.t =
  struct

      type t = | Interval of Endpoint.t * Endpoint.t
               | Empty
      with sexp

      let create low high =
         ...

      (* put a wrapper round the autogenerated sexp_of_t to enforce
         the invariants of the datastructure *)
      let t_of_sexp sexp =
        match t_of_sexp sexp with
        | Empty -> Empty
        | Interval (x,y) -> create x y

      ....

     end ;;
module Make_interval :
  functor
    (Endpoint : sig
           type t
           val compare : t -> t -> int
           val sexp_of_t : t -> Sexplib.Sexp.t
           val t_of_sexp : Sexplib.Sexp.t -> t
         end) ->
    sig
      type t
      val create : Endpoint.t -> Endpoint.t -> t
      val is_empty : t -> bool
      val contains : t -> Endpoint.t -> bool
      val intersect : t -> t -> t
      val sexp_of_t : t -> Sexplib.Sexp.t
      val t_of_sexp : Sexplib.Sexp.t -> t
    end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

And now, we can use that sexp-converter in the ordinary way:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Int = Make_interval(Int) ;;
# Int_interval.sexp_of_t (Int_interval.create 3 4);;
- : Sexplib.Sexp.t = (Interval 3 4)
# Int_interval.sexp_of_t (Int_interval.create 4 3);;
- : Sexplib.Sexp.t = Empty
~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Extending modules

One common use of functors is to generate type-specific functionality
for a given module in a standardized way.  We'll think about this in
the context of an example of creating a simple data structure.

The following is a minimal interface for a functional queue.  A
functional queue is simple a functional version of a FIFO (first-in,
first-out) queue.  Being functional, operations on the queue return
new queues, rather than modifying the queues that were passed in.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* file: fqueue.mli *)

type 'a t
val empty : 'a t
val enqueue : 'a t -> 'a -> 'a t
(** [dequeue q] returns None if the [q] is empty *)
val dequeue : 'a t -> ('a * 'a t) option
val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A standard trick for implementing functional queues efficiently is to
maintain both an input and an output list, where the input list is
ordered to make `enqueue` fast, and the output list is ordered to make
`dequeue` fast.  When the output list is empty, the input list is
reveresed and becomes the new output list. Thinking through why this
is efficient is a worthwhile exercise, but we won't dwell on that
here.

Here's a concrete implementation.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* file: fqueue.ml *)

type 'a t = 'a list * 'a list

let empty = ([],[])

let enqueue (l1,l2) x = (x :: l1,l2)

let dequeue (in_list,out_list) =
  match out_list with
  | hd :: tl -> Some (hd, (in_list,tl))
  | [] ->
    match List.rev in_list with
    | [] -> None
    | hd::tl -> Some (hd, ([], tl))

let fold (in_list,out_list) ~init ~f =
  List.fold ~init:(List.fold ~init ~f out_list) ~f
     (List.rev in_list)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The code above works fine, but the interface it implements is
unfortunately quite skeletal; there are lots of useful helper
functions that one might want that aren't there.  And implementing
those helper functions can be something of a dull affair, since you
need to implement essentially the same helper functions for multiple
different data structures in essentially the same way.

As it happens, many of these helper functions can be derived
mechanically from just the fold function we already implemented.
Rather than write all of these helper functions by hand for every new
container type, we can instead use a functor to write the code for
these once and for all, basing them off of the `fold` function.

Let's create a new module, `Foldable`, that contains support for this.
The first thing we'll need is a signature to describe a container that
supports fold.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* file: foldable.ml *)

module type S = sig
  type 'a t
  val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We'll also need a signature for the helper functions we're going to
generate.  This just represents some of the helper functions we can
derive from fold, but it's enough to give you a flavor of what you can
do.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
module type Extension = sig
  type 'a t
  val iter    : 'a t -> f:('a -> unit) -> unit
  val length  : 'a t -> int
  val count   : 'a t -> f:('a -> bool) -> int
  val for_all : 'a t -> f:('a -> bool) -> bool
  val exists  : 'a t -> f:('a -> bool) -> bool
end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Finally, we can define the functor itself.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
module Extend(Container : S)
  : Extension with type 'a t := 'a C.t =
struct
  open Container

  let iter   t ~f = fold t ~init:() ~f:(fun () a -> f a)
  let length t    = fold t ~init:0  ~f:(fun acc _ -> acc + 1)
  let count  t ~f = fold t ~init:0  ~f:(fun count x -> count + if f x then 1 else 0)

  exception Short_circuit

  let for_all c ~f =
    try iter c ~f:(fun x -> if not (f x) then raise Short_circuit); true
    with Short_circuit -> false

  let exists c ~f =
    try iter c ~f:(fun x -> if f x then raise Short_circuit); false
    with Short_circuit -> true
end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now we can apply this to `Fqueue`.  First, we can extend the interface:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* file: fqueue.mli, 2nd version *)

type 'a t
val empty : 'a t
val enqueue : 'a t -> 'a -> 'a t
val dequeue : 'a t -> ('a * 'a t) option
val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc

include Foldable.Extension with type 'a t := 'a t
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to apply the functor, we'll put the definition of `Fqueue` in
a sub-module called `T`, and then call `Foldable.Extend` on `T`.
Here's how that code would look.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
module T = struct
  type 'a t = 'a list * 'a list

  ....

  let fold (in_list,out_list) ~init ~f =
    List.fold ~init:(List.fold ~init ~f out_list)
      ~f (List.rev in_list)

end
include T
include Foldable.Extend(T)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This pattern comes up quite a bit in Core.  It's used to implement
various standard bits of functionality, including:

- Comparison-based datastructures like
  maps and sets, based on the `Comparable` interface.
- Hash-based datastructures like hash sets and hash heaps.
- Support for so-called monadic libraries, like the ones discussed in
  {{{ERROR HANDLING}}} and {{{CONCURRENCY}}}.  Here, the functor is
  used to provide a collection of standard helper functions based on
  the core `bind` and `return` operators.


## First class modules

You can think of OCaml as being broken up into two sub-language: a
core language that is concerned with values and types, and a module
language that is concerned with modules and module signatures.  These
sub-languages are stratified, in that modules can contain types and
values, but ordinary values can't contain modules or module types.
That means you can't do things like define a variable whose definition
is a module, or a function that takes a module as an argument.

OCaml provides a way around this stratification in the form of
_first-class modules_.  First-class modules are ordinary values that
can be created from and converted back to regular modules.  As we'll
see, letting modules into the core language makes it possible to use
more flexible and dynamic module-oriented designs.

### Another trivial example

Much as we did with functors, we'll start out with an utterly trivial
example, to allow us to show the basic mechanics of first class
modules with a minimum of fuss.

A first-class module is created by packaging up a module with a
signature that it satisfies.  The following defines a simple signature
and a module that matches it.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module type X_int = sig val x : int end;;
module type X_int = sig val x : int end
# module Three : X_int = struct let x = 3 end;;
module Three : X_int
# Three.x;;
- : int = 3
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can then create a first-class module using the `module` keyword.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let three = (module Three : X_int);;
val three : (module X_int) = <module>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the type of the first-class module, `(module X_int)`, is
based on the name of the signature that we used in constructing it.

To get at the contents of `three`, we need to unpack it into a module
again, which we can do using the `val` keyword.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module New_three = (val three : X_int) ;;
module New_three : X_int
# New_three.x;;
- : int = 3
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Using these conversions as building blocks, we can create tools for
working with first-class modules in a natural way.  The following
shows the definition of two function, `to_int`, which converts a
`(module X_int)` into an `int`.  And `plus`, which adds two `(module
X_int)`s.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let to_int m =
    let module M = (val m : X_int) in
    M.x
  ;;
val to_int : (module X_int) -> int = <fun>
# let plus m1 m2 =
    (module struct
       let x = to_int m1 + to_int m2
     end : X_int)
  ;;
val plus : (module X_int) -> (module X_int) -> (module X_int) = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

With these functions in hand, we can start operating on our `(module
X_int)`'s in a more natural style, taking full advantage of the
concision and simplicity of the core langauge.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let six = plus three three;;
val six : (module X_int) = <module>
# to_int (List.fold ~init:six ~f:plus [three;three]);;
- : int = 12
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Of course, all we've really done with this example is come up with a
more cumbersome way of working with integers.  To see the power of
first class modules, we'll need to look at a more realistic example.

### Dynamically choosing a module

Perhaps the simplest thing you can do with first-class modules that
you can't do without them is to pick the implementation of a module at
runtime.  

Consider an application that does I/O multiplexing using a system call
like `select` to determine which file descriptors are ready to use.
There are in fact multiple APIs you might want to use, including
`select` itself, `epoll`, and `libev`, where different multiplexers
make somewhat different performance and portability trade-offs.  You
could support all of these in one application by defining a single
module, let's call it `Mutliplexer`, whose implementation is chosen at
run-time based on an environment variable.

To do this, you'd first need an interface `S` that all of the
different multiplexer implementations would need to match, and then an
implementation of each multiplexer.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* file: multiplexer.ml *)

(* An interface the OS-specific functionality *)
module type S = sig ... end

(* The implementations of each individual multiplexer *)
module Select : S = struct ... end  
module Epoll  : S = struct ... end
module Libev  : S = struct ... end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can choose the first-class module that we want based on looking up
an environment variable.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
let multiplexer =
  match Sys.getenv "MULTIPLEXER" with
  | None
  | Some "select" -> (module Select : S)
  | Some "epoll"  -> (module Epoll : S)
  | Some "libev"  -> (module Libev : S)
  | Some other -> failwithf "Unknown multiplexer: %s" other ()
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Finally, we can convert the resulting first-class module back to an
ordinary module, and then include that so it becomes part of the body
of our module.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* The final, dynamically chosen, implementation *)
include (val multiplexer : S)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Example: A service bundle

This section describe the design of a library for bundling together
multiple services, where a service is a piece of code that exports a
query interface.  A service bundle combines together multiple
individual services under a single query interface that works by
dispatching incoming queries to the appropriate underlying service.

The following is a first attempt at an interface for our `Service`
module, which contains both a module type `S`, which is the interface
that a service should meet, as well as a `Bundle` module which is for
combining multiple services.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* file: service.mli *)

open Core.Std

(** The module type for a service. *)
module type S = sig
  type t
  val name           : string
  val create         : unit -> t
  val handle_request : t -> Sexp.t -> Sexp.t Or_error.t
end

(** Bundles multiple services together *)
module Bundle : sig
  type t
  val create : (module S) list -> t
  val handle_request : t -> Sexp.t -> Sexp.t Or_error.t
  val service_names  : t -> string list
end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here, a service has a state, represented by the type `t`, a name by
which the service can be referenced, a function `create` for
instantiating a service, and a function by which a service can
actually handle a request.  Here, requests and responses are delivered
as s-expressions.  At the `Bundle` level, the s-expression of a
request is expected to be formatted as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~
(<service-name> <body>)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

where `<service_name>` is the service that should handle the request,
and `<body>` is the body of the request.

Now let's look at how to implement `Service`.  The core datastructure
of `Bundle` is a hashtable of request handlers, one per service.
Each request handler is a function of type `(Sexp.t -> Sexp.t
Or_error.t)`.  These request handlers really stand in for the
underlying service, with the particular state of the service in
question being hidden inside of the request handler.

The first part of `service.ml` is just the preliminaries: the
definition of the module type `S`, and the definition of the type
`Bundle.t`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* file: service.ml *)

open Core.Std

module type S = sig
  type t
  val name           : string
  val create         : unit -> t
  val handle_request : t -> Sexp.t -> Sexp.t Or_error.t
end

module Bundle = struct
  type t = { handlers: (Sexp.t -> Sexp.t Or_error.t) String.Table.t; }
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The next thing we need is a function for creating a `Bundle.t`.  This
`create` function builds a table to hold the request handlers, and
then iterates through the services, unpacking each module,
constructing the request handler, and then putting that request
handler in the table.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
  (** Creates a handler given a list of services *)
  let create services =
    let handlers = String.Table.create () in
    List.iter services ~f:(fun service_m ->
      let module Service = (val service_m : S) in
      let service = Service.create () in
      if Hashtbl.mem handlers Service.name then
        failwith ("Attempt to register duplicate handler for "^Service.name);
      Hashtbl.replace handlers ~key:Service.name
        ~data:(fun sexp -> Service.handle_request service sexp)
    );
    {handlers}
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the `Service.t` that is created is referenced by the
corresponding request handler, so that it is effectively hidden behind
the function in the `handlers` table.

Now we can write the function for the bundle to handle requests.  The
handler will examine the s-expression to determine the body of the
query and the name of the service to dispatch to.  It then looks up
the hnalder calls it to generate the response.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
  let handle_request t sexp =
    match sexp with
    | Sexp.List [Sexp.Atom name;query] ->
      begin match Hashtbl.find t.handlers name with
      | None -> Or_error.error_string ("Unknown service: "^name)
      | Some handler ->
        try handler query
        with exn -> Error (Error.of_exn exn)
      end
    | _ -> Or_error.error_string "Malformed query"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Last of all, we define a function for looking up the names of the
available services.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
  let service_names t = Hashtbl.keys t.handlers

end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

To see this system in action, we need to define some services, create
the corresponding bundle, and then hook that bundle up to some kind of
client.  For simplicity, we'll build a simple command-line interface.
There are two functions below: `handle_one`, which handles a single
interaction; and `handle_loop`, which creates the bundle and then runs
`handle_one` in a loop.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* file: service_client.ml *)

open Core.Std

(** Handles a single request coming from stdin *)
let handle_one bundle =
  printf ">>> %!"; (* prompt *)
  match In_channel.input_line stdin with
  | None -> `Stop (* terminate on end-of-stream, so Ctrl-D will exit *)
  | Some line ->
    let line = String.strip line in (* drop leading and trailing whitespace *)
    if line = "" then `Continue
    else match Or_error.try_with (fun () -> Sexp.of_string line) with
    | Error err ->
      eprintf "Couldn't parse query: %s\n%!" (Error.to_string_hum err);
      `Continue
    | Ok query_sexp ->
      let resp = Service.Bundle.handle_request bundle query_sexp in
      Sexp.output_hum stdout (<:sexp_of<Sexp.t Or_error.t>> resp);
      Out_channel.newline stdout;
      `Continue

let handle_loop services =
  let bundle = Service.Bundle.create services in
  let rec loop () =
    match handle_one bundle with
    | `Stop -> ()
    | `Continue -> loop ()
  in
  loop ()
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now we'll create a couple of toy services.  One service is a counter
that can be updated by query; and the other service lists a directory.
The last line then kicks off the shell with the services we've
defined.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
module Counter : Service.S = struct
  type t = int ref

  let name = "update-counter"
  let create () = ref 0

  let handle_request t sexp =
    match Or_error.try_with (fun () -> int_of_sexp sexp) with
    | Error _ as err -> err
    | Ok x ->
      t := !t + x;
      Ok (sexp_of_int !t)
end

module List_dir : Service.S = struct
  type t = unit

  let name = "ls"
  let create () = ()

  let handle_request () sexp =
    match Or_error.try_with (fun () -> string_of_sexp sexp) with
    | Error _ as err -> err
    | Ok dir -> Ok (Array.sexp_of_t String.sexp_of_t (Sys.readdir dir))
end

let () =
  handle_loop [(module List_dir : Service.S); (module Counter : Service.S)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

And now we can go ahead and start up the client.

~~~~~~~~~~~~~~~~~~~~~~~~~~~
$ ./service_client.byte
>>> (update-counter 1)
(Ok 1)
>>> (update-counter 10)
(Ok 11)
>>> (ls .)
(Ok
 (_build _tags service.ml service.mli service.mli~ service.ml~
  service_client.byte service_client.ml service_client.ml~))
>>>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now, let's consider what happens to the design when we want to make
the interface of a service a bit more realistic.  In particular, right
now services are created without any configuration.  Let's add a
config type to each service, and change the interface of `Bundle` so
that services can be registered along with their configs.  At the same
time, we'll change the `Bundle` API to allow services to be changed
dynamically, rather than just added at creation time.
