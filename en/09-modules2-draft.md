# Functors and First-class modules

_(yminsky: Highly preliminary)_

Up until now, we've seen modules play only a limited role as a way of
organizing code into units with a mechanism for specifying interfaces
for those units.  But OCaml's modules serve a broader and deeper role,
acting as a powerful toolset for structuring larger-scale systems.  In
this chapter, we'll try to give you a taste of the power of that
system and show ways of using it effectively.

## Functors

A functor is, roughly speaking, a function from modules to modules.
Functors can be used to solve a variety of code-structuring problems,
including:

* _Dependency injection_, or making the implementations of some
  components of a system swappable.  This is particularly useful when
  you want to mock up parts of your system for testing and simulation
  purposes.
* _Auto-extension of modules_.  Sometimes, there is some functionality
  that you want to build in a standard way for many different types,
  based on a core piece of type-specific logic.  For example, you
  might want to add a slew of comparison operators derived from a base
  comparison function.  To do this by hand would require a lot of
  repetitive code for each type, but functors let you write this logic
  once and for all.C
* _Instantiating modules with state_.  Modules can contain mutable
  state, and that means that you'll occasionally want to have multiple
  instantiations of a particular module, each with its own independent
  state.  Functors let you automate the construction of such modules.

In the following, we'll walk through the basics of how functors work,
after which we'll look at examples of how to use functors effectively
in your software designs.

### A trivial functor

We'll start explaining functors by playing around with some simple
examples, starting with a very simple example indeed: a functor for
incrementing an integer.

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

Functors require more explicit type annotations than ordinary
functions.  In particular, the module type on the input module of a
functor is mandatory.  The module type on the output is not, however,
and will be inferred if omitted.

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

### An example: Intervals

This example will show how to build a library for operating on
intervals that is functorized over the type of the endpoints of the
intervals and the ordering of the endpoints.

First we'll define a module type that captures the information we'll
need about the endpoints.  This interface, which we call `Comparable`,
contains just two things: the type of the endpoint, and a
corresponding comparison function.  Here, we follow the convention
that the type that the module is about is called `t`.

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
represent an interval with a variant type, which is either `Empty`, if
the interval is empty, or `Interval (x,y)` where `x` and `y` are the
lower and upper bounds of the interval, respectively.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Make_interval(C : Comparable) = struct

    type t = | Interval of C.t * C.t
             | Empty

    let create low high =
      if C.compare low high > 0 then Empty
      else Interval (low,high)

    let is_empty = function
      | Empty -> true
      | Interval _ -> false

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
module Make_interval :
  functor (C : Comparable) ->
    sig
      type t = Interval of C.t * C.t | Empty
      val create : C.t -> C.t -> t
      val contains : t -> C.t -> bool
      val intersect : t -> t -> t
    end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

And now, we can instantiate the functor by applying it to a module
with the right signature.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# module Int_interval = Make_interval(struct
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

Note that if we choose our interfaces to be aligned with the standards
of the rest of our codebase, then we often don't have to construct a
custom module for a given functor.  In this case, for example, we can
directly use the `Int` or `String` modules provided by Core.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Int_interval = Make_interval(Int) ;;
# module String_interval = Make_interval(String) ;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This works because many modules in Core, including `Int` and `String`
satisfy an extended version of the `Comparable` signature described
above.  As a general matter, having standardized signatures is a good
practice, both because it makes your codebase more uniform and easier
to navigatge, and because it makes functors easier to use.

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

We're not locked in to using the standard comparison function is for
the endpoint type.  We could, for example, create a type of int
interval with the order of the comparison reversed, as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Rev_int_interval = Make_interval(struct
    type t = int
    let compare x y = Int.compare y x
  end);;
module Rev_int_interval :
  sig
    type t = Interval of int * int | Empty
    val create : int -> int -> t
    val is_empty : t -> bool
    val contains : t -> int -> bool
    val intersect : t -> t -> t
  end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The behavior of these intervals is of course different, as we can see
below.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let i1 = Int_interval.create 4 3;;
val i1 : Int_interval.t = Int_interval.Empty
# let i2 = Rev_int_interval.create 4 3;;
val i2 : Rev_int_interval.t = Rev_int_interval.Interval (4, 3)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that `i1` and `i2` are of different types, which is important,
since they are defined based on different comparison functions.
Indeed, if we try to operate on them jointly, we'll get an error:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Int_interval.intersect i1 i2;;
Characters 26-28:
  Int_interval.intersect i1 i2;;
                            ^^
Error: This expression has type Rev_int_interval.t
       but an expression was expected of type Int_interval.t
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This highlights an important feature functors, which is the ability to
mint new types.

#### Making the functor abstract

There's a problem with `Make_interval`.  The code we wrote depends on
the invariant that the upper bound of an interval is greater than its
lower bound, but that invariant can be violated.  The invariant is
enforced by the create function, but because `Interval.t` is not
abstract, we can bypass the `create` function, thus violating the
invariant.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Int_interval.create 4 3;; (* going through create *)
- : Int_interval.t = Int_interval.Empty
# Int_interval.Interval (4,3);; (* bypassing create *)
- : Int_interval.t = Int_interval.Interval (4, 3)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

To abstract the type `Int_interval.t`, we need to apply an interface
to the output of the functor.  Here's one possible such interface.
Note that we needed to add a new type, `endpoint`, to represent the
type of the endpoints of the interval.

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

Given this interface, we can redo our definition of `Make_interval`,
as follows.  Note that we have to add the type `endpoint` to the
implementation of the module to make it satisfy our new interface.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Make_interval(C : Comparable) : Interval_intf = struct

    type endpoint = C.t
    type t = | Interval of C.t * C.t
             | Empty

    ....

  end ;;
module Make_interval : functor (C : Comparable) -> Interval_intf
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The resulting module is abstract, but unfortunately, it's too
abstract.  In particular, we haven't exposed the type `endpoint`,
which means that we can't even construct an interval.

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
`Int.t` (or more generally, `C.t`, where `C` is the argument to the
functor).  There are two ways of doing this.  The first is a _sharing
constraint_, which simply tells the compiler that the two types in
question are equal.  The syntax for a sharing constraint on a module
type is:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
S with type t = s
~~~~~~~~~~~~~~~~~~~~~~~~~~~

where `S` is a module type, `t` is a type inside of `S`, and `s` is a
different type.  Here's how we would use a sharing constraint with
`Make_interval`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Make_interval(C : Comparable)
      : Interval_intf with type endpoint = C.t = struct

    type endpoint = C.t
    type t = | Interval of C.t * C.t
             | Empty

    ...

  end ;;
module Make_interval :
  functor (C : Comparable) ->
    sig
      type t
      type endpoint = C.t
      val create : endpoint -> endpoint -> t
      val is_empty : t -> bool
      val contains : t -> endpoint -> bool
      val intersect : t -> t -> t
    end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

So now, the interface is as it was, except that `endpoint` is now
known to be equal to `C.t`.

Sharing constraints basically do the job, but they're not perfect.  In
particular, we've now been stuck with this extra `endpoint` type that
clutters up both the interface and the implementation.  Ideally, we'd
just modify the signature by replacing `endpoint` with `C.t`
everywhere it shows up, making `endpoint` unnecessary.  We can do just
this using what's called _destructive substitution_.  Here's the basic
syntax.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
S with type t := s
~~~~~~~~~~~~~~~~~~~~~~~~~~~

where `S` is a signature, `t` is a type inside of `S`, and `s` is a
different type.  The following shows how we could use this with
`Make_interval`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module Make_interval(C : Comparable)
    : Interval_intf with type endpoint := C.t = struct

    type t = | Interval of C.t * C.t
             | Empty

    ....

  end ;;
module Make_interval :
  functor (C : Comparable) ->
    sig
      type t
      val create : C.t -> C.t -> t
      val is_empty : t -> bool
      val contains : t -> C.t -> bool
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
# module Make_interval(C : Comparable)
    : Interval_intf with type endpoint := C.t = struct

    type t = | Interval of C.t * C.t
             | Empty
    with sexp

    ....

  end ;;
Characters 120-123:
        type t = | Interval of C.t * C.t
                               ^^^
Error: Unbound value C.t_of_sexp
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The problem is that `with sexp` adds code for defining the
s-expression converters, and that code assumes that `C` has the
appropriate sexp-conversion functions for `C.t`.  But all we know
about `C` is that it satisfies the `Comparable` interface, which
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

Here's a version of `Make_interval` that supports s-expressions.  Note
here that we are careful to override the sexp-converter here to ensure
that the datastructures invariants are still maintained when reading
in from an s-expression.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# module type Interval_intf_with_sexp = sig
   type t
   include Interval_intf with type t := t
   include Sexpable      with type t := t
  end;;
# module Make_interval(C : sig
    type t
    include Comparable with type t := t
    include Sexpable   with type t := t
  end)
    : Interval_intf_with_sexp with type endpoint := C.t = struct

      type t = | Interval of C.t * C.t
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
    (C : sig
           type t
           val compare : t -> t -> int
           val sexp_of_t : t -> Sexplib.Sexp.t
           val t_of_sexp : Sexplib.Sexp.t -> t
         end) ->
    sig
      type t
      val create : C.t -> C.t -> t
      val is_empty : t -> bool
      val contains : t -> C.t -> bool
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~



## Detritus

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
