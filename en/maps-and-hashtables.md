# Maps and Hashtables

Organizing data as key/value pairs representing bindings of keys to
values is a useful and common programming pattern.  Maybe the simplest
way of representing a collection of bindings is as a list of pairs,
_i.e._, an _association list_.  Thus, you could write:

```ocaml
# let digit_alist =
    [ 0, "zero"; 1, "one"; 2, "two"; 3, "three"; 4, "four"
    ; 5, "five" ; 6, "six"; 7, "seven"; 8, "eight"; 9, "nine" ]
  ;;
```

And we can use functions from the `List.Assoc` module to manipulate
such an association list.

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

Association lists are simple and easy to use, and have the advantage
of being an immutable data-structure.  But the performance of
association lists is poor for most applications, since almost every
non-trivial operation on an association list requires a linear-time
scan of the list.

In this chapter, we'll talk about two more efficient alternatives to
association lists: _maps_ and _hash-tables_.  A map is an immutable
tree-based data structure where most operations on a map are take time
logarithmic in the size of the map; a hash-table is a mutable data
structure where most operations take constant time.  These are both
very useful and very common tools.  In the following, we'll describe
them both in some detail, and provide some advice as to when to use
one or the other.

## Constructing maps

Maps store their data in an ordered binary tree.  As such you need to
know how to compare elements of the key type in order to construct the
map.  Accordingly, functions for creating a map require that you pass
in a value of type `Comparator.t`, which is available as part of any
module that satisfies Core's `Comparable` interface.  Thus, we can
create a map as follows.

```ocaml
# let digit_map = Map.of_alist_exn Int.comparator digit_alist;;
val digit_map : (int, string, Int.comparator) Map.t = <abstr>
# Map.find digit_map 3;;
- : string option = Some "three"
# let digit_map' = Map.add digit_map 0 "zip";;
val digit_map' : (int, string, Int.comparator) Map.t = <abstr>
# (Map.find digit_map' 0, Map.find digit_map 0);;
- : string option * string option = (Some "zip", Some "zero")
```

We use the function `Map.of_alist_exn` to construct our map.  This
function throws an exception if it encounters entries with duplicate
keys.  Note that we only need to include the comparator on functions
that create new maps from scratch.  That's because the comparator is
stashed inside the map, so it's available within the data-structure
when you need to do a lookup or add a binding to a map.

We don't actually need to grab the comparator explicitly: the
`Comparable` interface provides a type-specialized version of the
`Map` module, so we can write:

```ocaml
# let digit_map = Int.Map.of_alist_exn digit_alist;;
val digit_map : string Int.Map.t = <abstr>
```

If we don't want to go to the trouble of using the specialized
comparison for a given type, we can use OCaml's build in polymorphic
compare as well, as shown below.

```ocaml
# Map.of_alist_exn Comparator.Poly.comparator digit_alist;;
- : (int, string, Comparator.Poly.comparator) Map.t = <abstr>
```

Or, more simply, and equivalently:

```ocaml
# Map.Poly.of_alist_exn digit_alist;;
- : (int, string) Map.Poly.t = <abstr>
```

All of these different construction methods end up returning maps of
the same polymorphic type, so that accessor functions like `Map.find`
or `Map.add` work no matter what.

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
</note>

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

## Working with maps

Maps come with a bunch of really useful primitives for interacting
with them.
