# Maps and Hashtables

Many problems solve for representing data as key/value pairs.  Maybe
the simplest way of doing so in OCaml is an _association list_,
_i.e._, a list of pairs of keys and values.  For example, you could
represent a mapping between the 10 digits and their English names as
follows.

```ocaml
# let digit_alist =
    [ 0, "zero"; 1, "one"; 2, "two"; 3, "three"; 4, "four"
    ; 5, "five" ; 6, "six"; 7, "seven"; 8, "eight"; 9, "nine" ]
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
association lists: _maps_ and _hash-tables_.  A map is an immutable
tree-based data structure where most operations take time logarithmic
in the size of the map, whereas a hash-table is a mutable data
structure where most operations have constant time complexity.  We'll
describe both of these data structures in detail, and provide some
advice as to how to choose between them.

## A map example

Let's start by considering an example of how one might use a map in
practice.  In [xref](#files-modules-and-programs), we showed a module
`Counter` for keeping counts on a set of strings.  Here's the
interface.

```ocaml
(* counter.mli *)
open Core.Std

type t

val empty : t
val to_list : t -> (string * int) list
val touch : t -> string -> t
```

The intended behavior here is straightforward.  `Counter.empty`
represents an empty collection of counts; `touch` increments the count
of the specified string by 1; and `to_list` returns the list of
non-zero string/count pairs.

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
implemented as ordered binary trees, and as such, maps need a way of
comparing keys.  To deal with this, a map actually stores the
necessary comparison function within the data structure.

Thus, operations that merely access the contents of an existing map,
like `Map.find`, do so by using the comparison function embedded
within the map.  Similarly, functions that generate a new map from an
old one, like `Map.add`, do the same.  

But in order to get a map in the first place, you need to get your
hands on the comparison function.  For this reason, modules like
`String` contain a `Map` sub-module that have values like
`String.Map.empty` and `String.Map.of_alist` that are specialized to
strings, and that in particular know how to compare strings.  Such a
`Map` sub-module is included in any module that satisfies the
`Comparable` interface from Core.

## Creating maps with comparators

The specialized `Map` sub-module is convenient, but you don't
necessarily need to use it to create a `Map.t`.  The information
required to compare values of a given type is wrapped up in a value
called a _comparator_, that can be used to create maps using the `Map`
module directly.

```ocaml
# let digit_map = Map.of_alist_exn digit_alist
                     ~comparator:Int.comparator;;
val digit_map : (int, string, Int.comparator) Map.t = <abstr>
# Map.find digit_map 3;;
- : string option = Some "three"
```

Here, we're use `Map.of_alist_exn` to construct our map.  This
function throws an exception if it encounters entries with duplicate
keys.

Note that the comparator is only required for operations that create
maps from scratch.  Operations that update an existing map simply
inherit the comparator of the map they start with.

```ocaml
# let zilch_map = Map.add digit_map ~key:0 ~data:"zilch";;
val zilch_map : (int, string, Int.comparator) Map.t = <abstr>
```

The type `Map.t` has three type parameters: one for the key, one for
the value, and one to identify the comparator used to build the map.
The type `'a Int.Map.t` is actually just a type alias for
`(int,'a,Int.comparator) Map.t`

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

If we look at the type of `Map.symmetric_diff`, we'll see that it
requires that the two maps it compares have the same comparator type,
in addition to the same key type and value type.  Each comparator has
a fresh abstract type, so the type of a comparator identifies it
uniquely.

```ocaml
# Map.symmetric_diff;;
- : ('k, 'v, 'cmp) Map.t ->
    ('k, 'v, 'cmp) Map.t ->
    data_equal:('v -> 'v -> bool) ->
    ('k * [ `Left of 'v | `Right of 'v | `Unequal of 'v * 'v ]) list
= <fun>
```

We could create a new comparator for an existing type using the `Make`
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

And if we use `Map.min_elt`, that returns the key and value for the
smallest key in the map, we can see that these two maps indeed do use
different comparison fucntions.

```ocaml
# Map.min_elt ord_map;;
- : (string * int) option = Some ("foo", 0)
# Map.min_elt rev_map;;
- : (string * int) option = Some ("snoo", 3)
```

If we try to compute the symmetric diff of these two maps, the
compiler will reject it as a type error.

```ocaml
# Map.symmetric_diff ord_map rev_map;;

Error: This expression has type (string, int, Reverse.comparator) Map.t
       but an expression was expected of type
         ('a, 'b, 'c) Map.t = (string, int, String.comparator) Map.t
       Type Reverse.comparator is not compatible with type String.comparator 
```

As we've mentioned, maps carry within them the comparator that they
were created with.  Sometimes, often for space efficiency reasons, you
want a version of the map data structure that doesn't include the
comparator.  You can get this using the `Map.to_tree` function, which
returns the tree underlying the map, without the comparator that was
used to create it.

```ocaml
# let ord_tree = Map.to_tree ord_map;; 
val ord_tree : (string, int, String.comparator) Map.Tree.t = <abstr>
```

Now, if we want to look something up in the tree, we need to provide
the comparator explicitly.

```ocaml
# Map.Tree.find ~comparator:String.comparator ord_tree "snoo";;
- : int option = Some 3
```

The algorithm of `Map.find` depends on the fact that you're using the
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

Again, the integration of comparators into the type of maps allows the
type system to catch what would otherwise be fairly subtle errors.

## Using the polymorphic comparator

We don't need to generate specialized comparison functions for every
type we encounter.  We can instead use a comparator based on OCaml's
polymorphic comparison function, which was discussed in
[xref](#lists-and-patterns).  Thus, we can write:

```ocaml
# Map.of_alist_exn Comparator.Poly.comparator digit_alist;;
- : (int, string, Comparator.Poly.comparator) Map.t = <abstr>
```

Or, equivalently:

```ocaml
# Map.Poly.of_alist_exn digit_alist;;
- : (int, string) Map.Poly.t = <abstr>
```

Note that maps based on the polymorphic comparator are not equivalent
to those based on the type-specific comparators, for good reasons:
there's no general guarantee that the comparator associated with a
given type will order things in the same way that polymorphic compare
does.

<warning> <title> The perils of polymorphic compare </title>

Polymorphic compare is highly convenient, but has serious downsides as
well, and should be used with care.  One issue is that polymorphic
compare is typically slower than type-specialized comparison
functions.  But more important than that is the fact that polymorphic
compare sometimes gives semantically surprising results.

To understand what's wrong with polymorphic comparison, you need to
know how it works.  Polymorphic compare operates directly on the
runtime-representation of OCaml objects, going field by field and
comparing items as they arise, not paying attention to the type of the
objects in question.

This is convenient because it works on most OCaml types, excluding
only things like closures and objects from outside of the OCaml heap
of the kind that arise from an external library binding.

But sometimes, a structural comparison is just not what you want.
Indeed, there are types for which you want two items to be considered
equal even if they're structurally different.  Maps are actually a
great example of this.  Consider the following.

```ocaml
# let (m1,m2) = (Int.Map.of_alist_exn [1,"one"; 2,"two"],
                 Int.Map.of_alist_exn [2,"two"; 1,"one"]);;
val m1 : string Int.Map.t = <abstr>
val m2 : string Int.Map.t = <abstr>
```

Logically, these two maps should be equal, since they have the same
set of key/value bindings.  But because the bindings were added in
different orders, the structure of the trees turns out to be
different, and so a structural comparison function will conclude that
they're different.

First, let's use the built in comparison function for maps.  Note
that we need to pass in an equality test for strings so that the
values can be compared as well.

```ocaml
# Map.equal String.equal m1 m2;;
- : bool = true
```

Now, let's try to do a polymorphic comparison, using the `=` operator.
Note that comparing the maps directly will fail at runtime because of
the comparators stored within the map contain function values.

```ocaml
# m1 = m2;;
Exception: (Invalid_argument "equal: functional value").
```

We can however use the function `Map.to_tree` to expose the underlying
tree without the attached comparator.

```ocaml
# Map.to_tree m1 = Map.to_tree m2;;
- : bool = false
```

These problems show up on any data structure where the natural
equality function is not structural.

</warning>

## Sets

Sometimes, instead of keeping track of a set of key/value pairs, you
just want a data-type for keeping track of a set set of keys.  You
could just build this on top of the map type, by representing a set of
values by a map where the type of the data is just `unit`.  A more
idiomatic (and more efficient) solution is to use Core's set type,
which is similar in design and spirit to the map type.  Here's a
simple example:

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
well as testing for whether one set is a subset of another.

# Hashtables

Hashtables are the imperative cousing of maps.  We walked over an
implementation of a hashtable in [xref](#imperative-programming-1), so
in this section we'll mostly discuss the pragmatics of Core's
`Hashtbl` module.


