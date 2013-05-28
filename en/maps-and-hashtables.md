# Maps and Hashtables

Lots of programming problems require dealing with data organized as
key/value pairs.  Maybe the simplest way of representing sets of
key/value pairs in OCaml is an _association list_, which is simply a
list of pairs of keys and values.  For example, you could represent a
mapping between the 10 digits and their English names as follows.

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
implemented as ordered binary trees, and as such, need a way of
comparing keys.

To deal with this, a map, once created, stores the necessary
comparison function within the data structure.  Thus, operations like
`Map.find` or `Map.add` that access the contents of a map, or create a
new map from an existing one, do so by using the comparison function
embedded within the map.

But in order to get a map in the first place, you need to get your
hands on the comparison function somehow.  For this reason, modules
like `String` contain a `Map` sub-module that have values like
`String.Map.empty` and `String.Map.of_alist` that are specialized to
strings, and this have access to a string comparison function.  Such a
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

The above uses `Map.of_alist_exn` which creates a map from an
association list, throwing an exception if there are duplicate keys in
the list.

Note that the comparator is only required for operations that create
maps from scratch.  Operations that update an existing map simply
inherit the comparator of the map they start with.

```ocaml
# let zilch_map = Map.add digit_map ~key:0 ~data:"zilch";;
val zilch_map : (int, string, Int.comparator) Map.t = <abstr>
```

The type `Map.t` has three type parameters: one for the key, one for
the value, and one to identify the comparator used to build the map.
Indeed, the type `'a Int.Map.t` is just a type alias for
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

And by using `Map.min_elt`, which returns the key and value for the
smallest key in the map, we can see that these two maps indeed do use
different comparison fucntions.

```ocaml
# Map.min_elt ord_map;;
- : (string * int) option = Some ("foo", 0)
# Map.min_elt rev_map;;
- : (string * int) option = Some ("snoo", 3)
```

Since the algorithm behind `Map.symmetric_diff` depends on both maps
using the same comparison function, we shouldn't be able to run this
function jointly on these two maps.  Indeed, if we try, the compiler
will reject it as a type error.

```ocaml
# Map.symmetric_diff ord_map rev_map;;

Error: This expression has type (string, int, Reverse.comparator) Map.t
       but an expression was expected of type
         ('a, 'b, 'c) Map.t = (string, int, String.comparator) Map.t
       Type Reverse.comparator is not compatible with type String.comparator 
```

As we've discussed, maps carry within them the comparator that they
were created with.  Sometimes, often for space efficiency reasons, you
want a version of the map data structure that doesn't include the
comparator.  You can get this with `Map.to_tree`, which returns the
tree type that the map is built on top of, which doesn't include the
comparator directly.

```ocaml
# let ord_tree = Map.to_tree ord_map;; 
val ord_tree : (string, int, String.comparator) Map.Tree.t = <abstr>
```

Note that although it doesn't physically include the comparator, it
still includes the comparator in its type.

If we want to look something up in a tree, we need to provide the
comparator explicitly.

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

The integration of comparators into the type of maps allows the type
system to catch what would otherwise be fairly subtle errors.

## The polymorphic comparator

We don't need to generate specialized comparators for every type we
want to build a map on.  We can instead use a comparator based on
OCaml's polymorphic comparison function, which was discussed in
[xref](#lists-and-patterns).  This comparator is found in the
`Map.Poly` module.  Thus, we can write:

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

<warning> <title> The perils of polymorphic compare </title>

Polymorphic compare is highly convenient, but it has serious downsides
as well, and should be used with care.  The problem with polymorphic
compare is that it has a fixed algorithm for comparing values of any
type, and that that algorithm can sometimes yield surprising results.

To understand what's wrong with polymorphic comparison, you need to
consider how it works.  Polymorphic compare is a structural
comparison, operating directly on the runtime-representation of OCaml
values, walking the structure of the values in question without regard
for their type.

This is convenient because it provides a comparison function that
behaves as you would expect for most OCaml types.  For example, on
`int`s and `float`s it acts as you would expect a numeric comparison
function to act.  For simple containers like strings and lists and
arrays it operates as a lexicographic comparator.  And it provides a
reasonable total comparison function for most OCaml types, excluding
only things like closures and values from outside of the OCaml heap.

But sometimes, a structural comparison is not what you want.  Maps and
Sets are a great example of this.  Consider the following two sets.

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
the trees trees will be different, and so a structural comparison
function will conclude that they're different.

Let's see what happens if we use polymorphic compare to test for
equality by way of the `=` operator.  Comparing the maps directly will
fail at runtime because of the comparators stored within the sets
contain function values.

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

For this reason, it's preferable to avoid polymorphic compare for all
but the smallest applications.

</warning>


# Hashtables

Hashtables are the imperative cousin of maps.  We walked over a basic
hashtable implementation in [xref](#imperative-programming-1), so in
this section we'll mostly discuss the pragmatics of Core's `Hashtbl`.

Hashtables differ from maps in a few key ways.  First, hashtables are
mutable, meaning that adding a key/value pair to a hashtable modifies
it, rather than creating a new table with the modification.  Second,
hashtables are have better time-complexity than maps, providing
constant time lookup and modifications to the table.  And finally,
just as maps depend on having a comparison function for creating the
ordered binary tree that underlies a map, hashtables depend on having
a _hash function_, i.e., a function for converting a key to an
integer.

Just as creating a map require a comparator, creating a hashtables
require a value of type `hashable`.  Thus, we can write:

```ocaml
# let table = Hashtbl.create ~hashable:String.hashable ();;
val table : (string, '_a) Hashtbl.t = <abstr>
# Hashtbl.replace table ~key:"three" ~data:3;;
- : unit = ()
# Hashtbl.find table "three";;
- : int option = Some 3
```

The `hashable` value is included as part of the `Hashable` interface,
which is satisfied by most types in Core.  The `Hashable` interface
also includes a `Table` sub-module which gives you more convenient
creation functions.

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

