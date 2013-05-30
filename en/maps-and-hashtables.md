# Maps and Hashtables

Lots of programming problems require dealing with data organized as
key/value pairs.  Maybe the simplest way of representing such data in
OCaml is an _association list_, which is simply a list of pairs of
keys and values.  For example, you could represent a mapping between
the 10 digits and their English names as follows.

```ocaml
# let digit_alist =
    [ 0, "zero"; 1, "one"; 2, "two"  ; 3, "three"; 4, "four"
    ; 5, "five"; 6, "six"; 7, "seven"; 8, "eight"; 9, "nine" ]
  ;;
```

We can use functions from the `List.Assoc` module to manipulate such
an association list.

```ocaml
# List.Assoc.find digit_alist 6;;
- : string option = Some "six"
# List.Assoc.find digit_alist 22;;
- : string option = None
# List.Assoc.add digit_alist 0 "zilch";;
- : (int, string) List.Assoc.t =
[(0, "zilch"); (1, "one"); (2, "two"); (3, "three"); (4, "four");
 (5, "five"); (6, "six"); (7, "seven"); (8, "eight"); (9, "nine")]
```

Association lists are simple and easy to use, but their performance is
not ideal, since almost every non-trivial operation on an association
list requires a linear-time scan of the list.

In this chapter, we'll talk about two more efficient alternatives to
association lists: _maps_ and _hashtables_.  A map is an immutable
tree-based data structure where most operations take time logarithmic
in the size of the map, whereas a hashtable is a mutable data
structure where most operations have constant time complexity.  We'll
describe both of these data structures in detail, and provide some
advice as to how to choose between them.

## Maps

Let's consider an example of how one might use a map in practice.  In
[xref](#files-modules-and-programs), we showed a module `Counter` for
keeping frequency counts on a set of strings.  Here's the interface.

```ocaml
(* counter.mli *)
open Core.Std

type t

val empty : t
val touch : t -> string -> t
val to_list : t -> (string * int) list
```

The intended behavior here is straightforward.  `Counter.empty`
represents an empty collection of frequency counts; `touch` increments
the frequency count of the specified string by 1; and `to_list`
returns the list of non-zero frequencies.

Here's the implementation.

```ocaml
(* counter.ml *)
open Core.Std

type t = int String.Map.t

let empty = String.Map.empty

let to_list t = Map.to_alist t

let touch t s =
  let count = Option.value ~default:0 (Map.find t s) in
  Map.add t ~key:s ~data:(count + 1)
```

Note that in some places the above code refers to `String.Map.t`, and
in others `Map.t`.  This has to do with the fact that maps are
implemented as ordered binary trees, and as such, need a way of
comparing keys.

To deal with this, a map, once created, stores the necessary
comparison function within the data structure.  Thus, operations like
`Map.find` or `Map.add` that access the contents of a map or create a
new map from an existing one, do so by using the comparison function
embedded within the map.

But in order to get a map in the first place, you need to get your
hands on the comparison function somehow.  For this reason, modules
like `String` contain a `Map` sub-module that have values like
`String.Map.empty` and `String.Map.of_alist` that are specialized to
strings, and thus have access to a string comparison function.  Such a
`Map` sub-module is included in every module that satisfies the
`Comparable.S` interface from Core.

### Creating maps with comparators

The specialized `Map` sub-module is convenient, but it's not the only
way of creating a `Map.t`.  The information required to compare values
of a given type is wrapped up in a value called a _comparator_, that
can be used to create maps using the `Map` module directly.

```ocaml
# let digit_map = Map.of_alist_exn digit_alist
                     ~comparator:Int.comparator;;
val digit_map : (int, string, Int.comparator) Map.t = <abstr>
# Map.find digit_map 3;;
- : string option = Some "three"
```

The above uses `Map.of_alist_exn` which creates a map from an
association list, throwing an exception if there are duplicate keys in
the list.

The comparator is only required for operations that create maps from
scratch.  Operations that update an existing map simply inherit the
comparator of the map they start with.

```ocaml
# let zilch_map = Map.add digit_map ~key:0 ~data:"zilch";;
val zilch_map : (int, string, Int.comparator) Map.t = <abstr>
```

The type `Map.t` has three type parameters: one for the key, one for
the value, and one to identify the comparator.  Indeed, the type `'a
Int.Map.t` is just a type alias for `(int,'a,Int.comparator) Map.t`

Including the comparator in the type is important because because
operations that work on multiple maps at the same time often require
that the maps share their comparison function.  Consider, for example,
`Map.symmetric_diff`, which computes a summary of the differences
between two maps.

```ocaml
# let left = String.Map.of_alist_exn ["foo",1; "bar",3; "snoo", 0]
  let right = String.Map.of_alist_exn ["foo",0; "snoo", 0]
  let diff = Map.symmetric_diff ~data_equal:Int.equal left right
  ;;
val left : int String.Map.t = <abstr>
val right : int String.Map.t = <abstr>
val diff :
  (string * [ `Left of int | `Right of int | `Unequal of int * int ]) list =
  [("foo", `Unequal (1, 0)); ("bar", `Left 3)]
```

The type of `Map.symmetric_diff`, shown below, requires that the two
maps it compares have the same comparator type.  Each comparator has a
fresh abstract type, so the type of a comparator identifies the
comparator uniquely.  

```ocaml
# Map.symmetric_diff;;
- : ('k, 'v, 'cmp) Map.t ->
    ('k, 'v, 'cmp) Map.t ->
    data_equal:('v -> 'v -> bool) ->
    ('k * [ `Left of 'v | `Right of 'v | `Unequal of 'v * 'v ]) list
= <fun>
```

This constis is important because the algorithm that
`Map.symmetric_diff` uses depends on the fact that both maps have the
same comparator.

We can create a new comparator for an existing type using the `Make`
functor found in the `Comparator` module.  This functor takes as its
input a module containing the type of the object to be compared,
sexp-converter functions, and a comparison function.  (The sexp
converters are included in the comparator to make it possible for
users of the comparator to generate better error messages.)

```ocaml
# module Reverse = Comparator.Make(struct
    type t = string
    let sexp_of_t = String.sexp_of_t
    let t_of_sexp = String.t_of_sexp
    let compare x y = String.compare y x
  end);;
module Reverse :
  sig
    type t = string
    val compare : t -> t -> int
    val t_of_sexp : Sexp.t -> t
    val sexp_of_t : t -> Sexp.t
    type comparator
    val comparator : (t, comparator) Comparator.t_
  end
```

Both `Reverse.comparator` and `String.comparator` can be used to
create string maps.

```ocaml
# let alist = ["foo", 0; "snoo", 3];;
val alist : (string * int) list = [("foo", 0); ("snoo", 3)]
# let ord_map = Map.of_alist_exn ~comparator:String.comparator alist;;
val ord_map : (string, int, String.comparator) Map.t = <abstr>
# let rev_map = Map.of_alist_exn ~comparator:Reverse.comparator alist;;
val rev_map : (string, int, Reverse.comparator) Map.t = <abstr>
```

`Map.min_elt` returns the key and value for the smallest key in the
map, which lets us see that the two maps we created do indeed use
different comparison fucntions.

```ocaml
# Map.min_elt ord_map;;
- : (string * int) option = Some ("foo", 0)
# Map.min_elt rev_map;;
- : (string * int) option = Some ("snoo", 3)
```

And accordingly, if we try to use `Map.symmetric_diff` on these two
maps, we'll get a compile-timer error.

```ocaml
# Map.symmetric_diff ord_map rev_map;;

Error: This expression has type (string, int, Reverse.comparator) Map.t
       but an expression was expected of type
         ('a, 'b, 'c) Map.t = (string, int, String.comparator) Map.t
       Type Reverse.comparator is not compatible with type String.comparator 
```

### Trees

As we've discussed, maps carry within them the comparator that they
were created with.  Sometimes, often for space efficiency reasons, you
want a version of the map data structure that doesn't include the
comparator.  You can get this with `Map.to_tree`, which returns the
tree that the map is built on top of, which doesn't include the
comparator.

```ocaml
# let ord_tree = Map.to_tree ord_map;; 
val ord_tree : (string, int, String.comparator) Map.Tree.t = <abstr>
```

Note that although it doesn't physically include the comparator, it
still includes the comparator in its type.  This is what is known as a
_phantom type_, because the type reflects something about the logic of
how the value was constructued, but the type doesn't correspond to any
values directly represented in the underlying data structure.

To look something up in a tree, we need to provide the comparator
explicitly.

```ocaml
# Map.Tree.find ~comparator:String.comparator ord_tree "snoo";;
- : int option = Some 3
```

The algorithm of `Map.Tree.find` depends on the fact that you're using the
same comparator when looking a value up as you were when you stored
it.  Accordingly, using the wrong comparator will lead to a type
error.

```ocaml
# Map.Tree.find ~comparator:Reverse.comparator ord_tree "snoo";;

Error: This expression has type (string, int, String.comparator) Map.Tree.t
       but an expression was expected of type
         ('a, 'b, 'c) Map.Tree.t = (string, 'b, Reverse.comparator) Map.Tree.t
       Type String.comparator is not compatible with type Reverse.comparator 
```

The integration of comparators into the type of maps allows the type
system to catch what would otherwise be fairly subtle errors.

### The polymorphic comparator

We don't need to generate specialized comparators for every type we
want to build a map on.  We can instead use a comparator based on
OCaml's polymorphic comparison function, which was discussed in
[xref](#lists-and-patterns).  This comparator is found in the
`Comparator.Poly` module.  Thus, we can write:

```ocaml
# Map.of_alist_exn ~comparator:Comparator.Poly.comparator digit_alist;;
- : (int, string, Comparator.Poly.comparator) Map.t = <abstr>
```

Or, equivalently:

```ocaml
# Map.Poly.of_alist_exn digit_alist;;
- : (int, string) Map.Poly.t = <abstr>
```

Note that maps based on the polymorphic comparator are not equivalent
to those based on the type-specific comparators from the point of view
of the type system.  Thus, the compiler rejects the following:

```ocaml
# Map.symmetric_diff (Map.Poly.singleton 3 "three")
                     (Int.Map.singleton  3 "four" ) ;;

Error: This expression has type 'a Int.Map.t = (int, 'a, Int.comparator) Map.t
       but an expression was expected of type
         ('b, 'c, 'd) Map.t = (int, string, Z.Poly.comparator) Map.t
       Type Int.comparator is not compatible with type Z.Poly.comparator 
```

This is rejected for good reason: there's no guarantee that the
comparator associated with a given type will order things in the same
way that polymorphic compare does.

### Sets

Sometimes, instead of keeping track of a set of key/value pairs, you
just want a data-type for keeping track of a set of keys.  You could
just build this on top of the map type, by representing a set of
values by a map where the type of the data is `unit`.  A more
idiomatic (and efficient) solution is to use Core's set type, which is
similar in design and spirit to the map type.  Here's a simple
example:

```ocaml
# let dedup ~comparator l =
    List.fold l ~init:(Set.empty ~comparator) ~f:Set.add
    |> Set.to_list
  ;;
val dedup : comparator:('a, 'b) Core.Comparator.t_ -> 'a list -> 'a list =
  <fun>
# dedup ~comparator:Int.comparator [8;3;2;3;7;8;10];;
- : int list = [2; 3; 7; 8; 10]
```

In addition to the operators you would expect to have in map, sets,
also support other operations you would expect for sets, including
functions for computing unions, intersections and set differences, as
well as testing for whether one set is a subset of another.  And, as
with maps, we can create sets based on type-specific comparators or on
the polymorphic comparato.rhe pr

<warning> <title> The perils of polymorphic compare </title>

Polymorphic compare is highly convenient, but it has serious downsides
as well, and should be used with care.  In particular, polymorphic
compare has a fixed algorithm for comparing values of any type, and
that algorithm can sometimes yield surprising results.

To understand what's wrong with polymorphic compare, you need to
understand a bit about how it works.  Polymorphic compare is
_structural_, in that it operates directly on the
runtime-representation of OCaml values, walking the structure of the
values in question without regard for their type.

This is convenient because it provides a comparison function that
works for most OCaml values, and largely behaves as you would expect.
For example, on `int`s and `float`s it acts as you would expect a
numeric comparison function to act.  For simple containers like
strings and lists and arrays it operates as a lexicographic
comparison.  And except for closures and values from outside of the
OCaml heap, it works on almost every OCaml type.

But sometimes, a structural comparison is not what you want.  Sets are
a great example of this.  Consider the following two sets.

```ocaml
# let (s1,s2) = (Int.Set.of_list [1;2],
                 Int.Set.of_list [2;1]);;
val s1 : Int.Set.t = <abstr>
val s2 : Int.Set.t = <abstr>
```

Logically, these two sets should be equal, and that's the result that
you get if you call `Set.equal` on them.

```ocaml
# Set.equal s1 s2;;
- : bool = true
```

But because the elements were added in different orders, the layout of
the trees underlying the sets will be different.  As such, a
structural comparison function will conclude that they're different.

Let's see what happens if we use polymorphic compare to test for
equality by way of the `=` operator.  Comparing the maps directly will
fail at runtime because the comparators stored within the sets contain
function values.

```ocaml
# s1 = s2;;
Exception: (Invalid_argument "equal: functional value").
```

We can however use the function `Set.to_tree` to expose the underlying
tree without the attached comparator.

```ocaml
# Set.to_tree s1 = Set.to_tree s2;;
- : bool = false
```

This can cause real and quite subtle bugs.  If, for example, you use a
map whose keys contain sets, then the map built with the polymorphic
comparator will behave incorrectly, separating out keys that should be
aggregated together.  Even worse, it will work sometimes and fail
others, since if the sets are built in a consistent order, then they
will work as expected, but once the order changes, the behavior will
change.

For this reason, it's preferable to avoid polymorphic compare for
serious applications.

</warning>

### Satisfying the `Comparable.S` interface

Core's `Comparable.S` interface includes a lot of useful support for
working with maps and sets, in particular, the presence of a
comparator as well as the `Map` and `Set` sub-modules.

`Comparable.S` is already satisfied by most of the modules in Core,
but the question arises of how to satisfy the comparable interface for
a custom type that you design, since implementing all of this
functionality from scratch would be an absurd amount of work.

The module `Comparable` contains a number of functors to help you do
just this.  The simplest one of these is `Comparable.Make`, which
takes as an input any module that satisfies the following interface:

```ocaml
module type Arg : sig
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
  val compare : t -> t -> int
end
```

In other words, it expects the input module to support a comparison
function with the default signature, and it expects it to have
functions for converting to and from s-expressions, a serialization
format that we'll discuss more in
[xref](#data-serialization-with-s-expressions).  In this case, we'll
use the `with sexp` declaration that comes from the `sexplib` syntax
extension to create s-expression converters for us, but such
converters can also be written by hand.

Here's an example of how you can make a type satisfy `Comparable.S`
using the `Comparable.Make` functor to generate most of the necessary
functionality.  This follows the same basic pattern for using functors
described in [xref](#extending-modules).  We do need to provide a
comparison function, though, which we'll just do by hand.

```ocaml
# module Foo_and_bar : sig
    type t = { foo: Int.Set.t; bar: string }
    include Comparable.S with type t := t
  end = struct
    module T = struct
      type t = { foo: Int.Set.t; bar: string } with sexp
      let compare t1 t2 =
        let c = Int.Set.compare t1.foo t2.foo in
        if c <> 0 then c else String.compare t1.bar t2.bar
    end
    include T
    include Comparable.Make(T)
  end;;
```

We don't include the full response from the top-level because it is
quite lengthy, but `Foo_and_bar` does satisfy `Comparable.S`.

We don't necessarily need to write the comparison function by hand.
Core ships with another syntax extension called `comparelib` which
will create a comparison function based on a type definition.  Using
it, we can rewrite the above example as follows without changing the
behavior.

```ocaml
# module Foo_and_bar : sig
    type t = { foo: Int.Set.t; bar: string }
    include Comparable.S with type t := t
  end = struct
    module T = struct
      type t = { foo: Int.Set.t; bar: string } with sexp, compare
    end
    include T
    include Comparable.Make(T)
  end;;
```

The comparison function created by `pa_compare` for a given type will
call out to the comparison functions for its component types, and so
the `foo` field will be compared using `Int.Set.compare` rather than
doing a structural comparison, as polymorphic compare would.  If you
want your comparison function to behave in a specific way, you should
still write your own comparison function by hand, but if all you want
is a total order suitable for creating maps and sets with, then
`pa_compare` is a good choice.

You can also satisfy the `Comparable.S` interface using polymorphic
compare, if that makes sense for the type in question.

```ocaml
# module Foo_and_bar : sig
    type t = { foo: int; bar: string }
    include Comparable.S with type t := t
  end = struct
    module T = struct
      type t = { foo: int; bar: string } with sexp
    end
    include T
    include Comparable.Poly(T)
  end;;
```

That said, for reasons we discussed earlier, polymorphic compare
should be used sparingly.

## Hashtables

Hashtables are the imperative cousin of maps.  We walked over a basic
hashtable implementation in [xref](#imperative-programming-1), so in
this section we'll mostly discuss the pragmatics of Core's `Hashtbl`
module.  We'll cover this material more briefly than we did with maps,
because many of the concepts are shared.

Hashtables differ from maps in a few key ways.  First, hashtables are
mutable, meaning that adding a key/value pair to a hashtable modifies
the table, rather than creating a new table with the binding added.
Second, hashtables generally have better time-complexity than maps,
providing constant time lookup and modifications as opposed to
logarithmic for maps.  And finally, just as maps depend on having a
comparison function for creating the ordered binary tree that
underlies a map, hashtables depend on having a _hash function_,
_i.e._, a function for converting a key to an integer.

When creating a hashtable, we need to provide value of type _hashable_
which includes among other things the function for hashing the key
type.  This is analogous to the comaprator used for creating maps.

```ocaml
# let table = Hashtbl.create ~hashable:String.hashable ();;
val table : (string, '_a) Hashtbl.t = <abstr>
# Hashtbl.replace table ~key:"three" ~data:3;;
- : unit = ()
# Hashtbl.find table "three";;
- : int option = Some 3
```

The `hashable` value is included as part of the `Hashable.S`
interface, which is satisfied by most types in Core.  The `Hashable.S`
interface also includes a `Table` sub-module which provides more
convenient creation functions.

```ocaml
# let table = String.Table.create ();;
val table : '_a String.Table.t = <abstr>
```

There is also a polymorphic `hashable` value, corresponding to the
polymorphic hash function provided by the OCaml runtime, for cases
where you don't have a hash function for your specific type.

```ocaml
# let table = Hashtbl.create ~hashable:Hashtbl.Poly.hashable ();;
val table : ('_a, '_b) Hashtbl.t = <abstr>
```

Or, equivalently:

```ocaml
# let table = Hashtbl.Poly.create ();;
val table : ('_a, '_b) Hashtbl.t = <abstr>
```

Note that, unlike the comparators used with maps and sets, hashables
don't show up in the type of a `Hashtbl.t`.  That's because hashtables
don't have operations that operate on multiple hashtables that depend
on those tables having the same hash function, in that way that
`Map.symmetric_diff` and `Set.union` depend on their arguments using
the same comparison function.

### Satisfying the `Hashable.S` interface

Most types in Core satisfy the `Hashable.S` interface, but as with the
`Comparable.S` interface, the question remains of how one should
satisfy this interface with a new type.  Again, the answer is to use a
functor to build the necessary functionality; in this case,
`Hashable.Make`.  Note that we use OCaml's `lxor` operator for doing
the "logical" (_i.e._, bit-wise) exclusive-or of the hashes from the
component values.

```ocaml
# module Foo_and_bar : sig
    type t = { foo: int; bar: string }
    include Hashable.S with type t := t
  end = struct
    module T = struct
      type t = { foo: int; bar: string } with sexp, compare
      let hash t =
        (Int.hash t.foo) lxor (String.hash t.bar)
    end
    include T
    include Hashable.Make(T)
  end;;
```

Note that in order to satisfy hashable, one also needs to provide a
comparison function.  That's because Core's hashtables use ordered
binary tree data-structure for the hash-buckets, so that performance
of the table degrades gracefully in the case of pathologically bad
choice of hash function.

There is currently no analogue of `comparelib` for auto-generation of
hash-functions, so you do need to either write the hash-function by
hand, or use the built-in polymorphic hash function, `Hashtbl.hash`.




