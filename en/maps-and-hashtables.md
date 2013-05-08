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

# Maps

Maps work by organizing their data in an ordered binary tree.  As
such, in order to create a map for a given type of tree, you need to
know how to compare any two keys.  When building a map, this comes
from grabbing a value of type `Int.comparator`, which is part of the
`Comparable` interface in Core.  Thus, we can create a map as follows:

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

