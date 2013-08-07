# Maps and Hash Tables

Lots of programming problems require dealing with data organized as
key/value pairs.  Maybe the simplest way of representing such data in
OCaml is an _association list_, which is simply a list of pairs of
keys and values.  For example, you could represent a mapping between
the 10 digits and their English names as follows.

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 1))
```

We can use functions from the `List.Assoc` module to manipulate such
an association list.

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 2))
```

Association lists are simple and easy to use, but their performance is
not ideal, since almost every non-trivial operation on an association
list requires a linear-time scan of the list.

In this chapter, we'll talk about two more efficient alternatives to
association lists: _maps_ and _hash tables_.  A map is an immutable
tree-based data structure where most operations take time logarithmic
in the size of the map, whereas a hash table is a mutable data
structure where most operations have constant time complexity.  We'll
describe both of these data structures in detail, and provide some
advice as to how to choose between them.

## Maps

Let's consider an example of how one might use a map in practice.  In
[xref](#files-modules-and-programs), we showed a module `Counter` for
keeping frequency counts on a set of strings.  Here's the interface.

```frag
((typ ocaml)(name files-modules-and-programs-freq-fast/counter.mli))
```

The intended behavior here is straightforward.  `Counter.empty`
represents an empty collection of frequency counts; `touch` increments
the frequency count of the specified string by 1; and `to_list`
returns the list of non-zero frequencies.

Here's the implementation.

```frag
((typ ocaml)(name files-modules-and-programs-freq-fast/counter.ml))
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

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 3))
```

The above uses `Map.of_alist_exn` which creates a map from an
association list, throwing an exception if there are duplicate keys in
the list.

The comparator is only required for operations that create maps from
scratch.  Operations that update an existing map simply inherit the
comparator of the map they start with.

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 4))
```

The type `Map.t` has three type parameters: one for the key, one for
the value, and one to identify the comparator.  Indeed, the type `'a
Int.Map.t` is just a type alias for `(int,'a,Int.comparator) Map.t`

Including the comparator in the type is important because operations
that work on multiple maps at the same time often require that the
maps share their comparison function.  Consider, for example,
`Map.symmetric_diff`, which computes a summary of the differences
between two maps.

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 5))
```

The type of `Map.symmetric_diff`, shown below, requires that the two
maps it compares have the same comparator type.  Each comparator has a
fresh abstract type, so the type of a comparator identifies the
comparator uniquely.  

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 6))
```

This constraint is important because the algorithm that
`Map.symmetric_diff` uses depends on the fact that both maps have the
same comparator.

We can create a new comparator using the `Comparator.Make` functor,
which takes as its input a module containing the type of the object to
be compared, sexp-converter functions, and a comparison function.  The
sexp converters are included in the comparator to make it possible for
users of the comparator to generate better error messages.  Here's an
example.

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 7))
```

As you can see below, both `Reverse.comparator` and
`String.comparator` can be used to create maps with a key type of
`string`.

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 8))
```

`Map.min_elt` returns the key and value for the smallest key in the
map, which lets us see that these two maps do indeed use different
comparison functions.

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 9))
```

And accordingly, if we try to use `Map.symmetric_diff` on these two
maps, we'll get a compile-timer error.

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 10))
```

### Trees

As we've discussed, maps carry within them the comparator that they
were created with.  Sometimes, often for space efficiency reasons, you
want a version of the map data structure that doesn't include the
comparator.  You can get such a representation with `Map.to_tree`,
which returns just the tree that the map is built out of, and not
including the comparator.

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 11))
```

Even though a `Map.Tree.t` doesn't physically include a comparator, it
does include the comparator in its type.  This is what is known as a
_phantom type parameter_, because it reflects something about the
logic of the value in question, even though it doesn't correspond to
any values directly represented in the underlying physical structure
of the value.

Since the comparator isn't included in the tree, we need to provide
the comparator explicitly when we, say, search for a key, as shown
below.

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 12))
```

The algorithm of `Map.Tree.find` depends on the fact that it's using
the same comparator when looking a value up as you were when you
stored it.  That's the invariant that the phantom type is there to
enforce.  As you can see below, using the wrong comparator will lead
to a type error.

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 13))
```

### The polymorphic comparator

We don't need to generate specialized comparators for every type we
want to build a map on.  We can instead use a comparator based on
OCaml's built-in polymorphic comparison function, which was discussed
in [xref](#lists-and-patterns).  This comparator is found in the
`Comparator.Poly` module, allowing us to write:

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 14))
```

Or, equivalently:

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 15))
```

Note that maps based on the polymorphic comparator are not equivalent
to those based on the type-specific comparators from the point of view
of the type system.  Thus, the compiler rejects the following:

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 16))
```

This is rejected for good reason: there's no guarantee that the
comparator associated with a given type will order things in the same
way that polymorphic compare does.

<note>
<title> `=`, `==`, and `phys_equal` </title>

If you come from a C/C++ background, you'll probably reflexively use
`==` to test two values for equality.  In OCaml, the `==` operator
tests for *physical* equality while the `=` operator tests for
*structural* equality.

The physical equality test will match if two data structures have
precisely the same pointer in memory.  Two data structures that have
identical contents but are constructed separately will not match using
`==`.

The `=` structural equality operator recursively inspects each field
in the two values and tests them individually for equality.
Crucially, if your data structure is cyclical (that is, a value
recursively points back to another field within the same structure),
the `=` operator will never terminate, and your program will hang!
You therefore must use the physical equality operator or write a
custom comparison function when comparing recursive values.

It's quite easy to mix up the use of `=` and `==`, so Core disables
the `==` operator and provides the more explicit `phys_equal` function
instead.  You'll see a type error if you use `==` anywhere in code
that opens `Core.Std`.

```frag
((typ ocamltop)(name maps-and-hash-tables/core_phys_equal.topscript))
```

If you feel like hanging your OCaml interpreter, you can verify what
happens with recursive values and structural equality for yourself:

```frag
((typ ocamlrawtop)(name maps-and-hash-tables/phys_equal.rawscript))
```

</note>

### Sets

Sometimes, instead of keeping track of a set of key/value pairs, you
just want a data-type for keeping track of a set of keys.  You could
build this on top of a map by representing a set of values by a map
whose data type is `unit`.  But a more idiomatic (and efficient)
solution is to use Core's set type, which is similar in design and
spirit to the map type, while having an API better tuned to working
with sets, and a lower memory footprint.  Here's a simple example:

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 17))
```

In addition to the operators you would expect to have for maps, sets
support the traditional set operations, including union, intersection
and set difference.  And, as with maps, we can create sets based on
type-specific comparators or on the polymorphic comparator.

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

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 18))
```

Logically, these two sets should be equal, and that's the result that
you get if you call `Set.equal` on them.

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 19))
```

But because the elements were added in different orders, the layout of
the trees underlying the sets will be different.  As such, a
structural comparison function will conclude that they're different.

Let's see what happens if we use polymorphic compare to test for
equality by way of the `=` operator.  Comparing the maps directly will
fail at runtime because the comparators stored within the sets contain
function values.

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 20))
```

We can however use the function `Set.to_tree` to expose the underlying
tree without the attached comparator.

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 21))
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

Core's `Comparable.S` interface includes a lot of useful
functionality, including support for working with maps and sets.  In
particular, `Comparable.S` requires the presence of the `Map` and
`Set` sub-modules as well as a comparator.

`Comparable.S` is satisfied by most of the types in Core, but the
question arises of how to satisfy the comparable interface for a new
type that you design.  Certainly implementing all of the required
functionality from scratch would be an absurd amount of work.

The module `Comparable` contains a number of functors to help you do
just this.  The simplest one of these is `Comparable.Make`, which
takes as an input any module that satisfies the following interface:

```frag
((typ ocaml)(name maps-and-hash-tables/comparable.ml))
```

In other words, it expects a type with a comparison function as well
as functions for converting to and from _s-expressions_.
S-expressions are a serialization format used commonly in Core, and
are required here to enable better error messages.  We'll discuss
s-expressions more in [xref](#data-serialization-with-s-expressions),
but in the meantime, we can just use the `with sexp` declaration that
comes from the `sexplib` syntax extension to create s-expression
converters for us.

The following example shows how this all fits together, following the
same basic pattern for using functors described in
[xref](#extending-modules).

```frag
((typ ocamlrawtop)(name maps-and-hash-tables/main-22.rawscript))
```

We don't include the full response from the top-level because it is
quite lengthy, but `Foo_and_bar` does satisfy `Comparable.S`.

In the above, we wrote the comparison function by hand, but this isn't
strictly necessary.  Core ships with a syntax extension called
`comparelib` which will create a comparison function from a type
definition.  Using it, we can rewrite the above example as follows.

```frag
((typ ocamlrawtop)(name maps-and-hash-tables/main-23.rawscript))
```

The comparison function created by `comparelib` for a given type will
call out to the comparison functions for its component types.  As a
result, the `foo` field will be compared using `Int.Set.compare`.
This is different, and saner, than the structural comparison done by
polymorphic compare.  

If you want your comparison function to behave in a specific way, you
should still write your own comparison function by hand; but if all
you want is a total order suitable for creating maps and sets with,
then `comparelib` is a good way to go.

You can also satisfy the `Comparable.S` interface using polymorphic
compare.

```frag
((typ ocamlrawtop)(name maps-and-hash-tables/main-24.rawscript))
```

That said, for reasons we discussed earlier, polymorphic compare
should be used sparingly.

## Hash tables

Hash tables are the imperative cousin of maps.  We walked over a basic
hash table implementation in [xref](#imperative-programming-1), so in
this section we'll mostly discuss the pragmatics of Core's `Hashtbl`
module.  We'll cover this material more briefly than we did with maps,
because many of the concepts are shared.

Hash tables differ from maps in a few key ways.  First, hash tables are
mutable, meaning that adding a key/value pair to a hash table modifies
the table, rather than creating a new table with the binding added.
Second, hash tables generally have better time-complexity than maps,
providing constant time lookup and modifications as opposed to
logarithmic for maps.  And finally, just as maps depend on having a
comparison function for creating the ordered binary tree that
underlies a map, hash tables depend on having a _hash function_,
_i.e._, a function for converting a key to an integer.

<warning> <title> Time complexity of hash tables </title>

The statement that hash tables provide constant-time access hides some
complexities.  First of all, any hash table implementation, OCaml's
included, needs to resize the table when it gets too full.  A resize
requires allocating a new backing array for the hash table and copying
over all entries, and so it is quite an expensive operation.  That
means adding a new element to the table is only _amortized_ constant,
which is to say, it's constant on average over a long sequence of
additions, but some of the individual additions can be quite expensive.

Another hidden cost of hash tables has to do with the hash function you
use.  If you end up with a pathologically bad hash function that
hashes all of your data to the same number, then all of your
insertions will hash to the same underlying bucket, meaning you no
longer get constant-time access at all.  Core's hash table
implementation uses binary trees for the hash-buckets, so this case
only leads to logarithmic time, rather than quadratic for a
traditional hash table.  

The logarithmic behavior of Core's hash tables in the presence of hash
collisions also helps protect against some denial-of-service attacks.
One well-known type of attack is to send queries to a service with
carefully chosen keys to cause many collisions.  This, in combination
with the the quadratic behavior of hash tables, can cause the service
to become unresponsive due to high CPU load.  Core's hash tables would
be much less susceptible to such an attack, because the amount of
degradation would be far less.

</warning>

When creating a hash table, we need to provide a value of type
_hashable_ which includes among other things the function for hashing
the key type.  This is analogous to the comparator used for creating
maps.

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 25))
```

The `hashable` value is included as part of the `Hashable.S`
interface, which is satisfied by most types in Core.  The `Hashable.S`
interface also includes a `Table` sub-module which provides more
convenient creation functions.

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 26))
```

There is also a polymorphic `hashable` value, corresponding to the
polymorphic hash function provided by the OCaml runtime, for cases
where you don't have a hash function for your specific type.

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 27))
```

Or, equivalently:

```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 28))
```

Note that, unlike the comparators used with maps and sets, hashables
don't show up in the type of a `Hashtbl.t`.  That's because hash
tables don't have operations that operate on multiple hash tables that
depend on those tables having the same hash function, in the way that
`Map.symmetric_diff` and `Set.union` depend on their arguments using
the same comparison function.

<warning> <title> Collisions with the polymorphic hash function </title>

OCaml's polymorphic hash function works by walking over the
data-structure its given using a breadth-first traversal that is
bounded in the number of nodes its willing to traverse.  By default,
that bound is set at 10 "meaningful" nodes, essentially...

The bound on the traversal, means that the hash function may ignore
part of the data-structure, and this can lead to pathological cases
where every value you store has the same hash value.  By default,
OCaml's hash function will stop after it has found ten nodes it can
extract data from.  We'll demonstrate this below, using the function
`List.range` to allocate lists of integers of different length.


```frag
((typ ocamltop)(name maps-and-hash-tables/main.topscript)(part 29))
```

As you can see, the hash function stops after the first 10 elements.
The same can happen with any large data structure, including records
and arrays.  When building hash functions over large custom
data-structures, it is generally a good idea to write one's own hash
function, _e.g._, 

</warning>


### Satisfying the `Hashable.S` interface

Most types in Core satisfy the `Hashable.S` interface, but as with the
`Comparable.S` interface, the question remains of how one should
satisfy this interface with a new type.  Again, the answer is to use a
functor to build the necessary functionality; in this case,
`Hashable.Make`.  Note that we use OCaml's `lxor` operator for doing
the "logical" (_i.e._, bit-wise) exclusive-or of the hashes from the
component values.

```frag
((typ ocamlrawtop)(name maps-and-hash-tables/main-30.rawscript))
```

Note that in order to satisfy hashable, one also needs to provide a
comparison function.  That's because Core's hash tables use an ordered
binary tree data-structure for the hash-buckets, so that performance
of the table degrades gracefully in the case of pathologically bad
choice of hash function.

There is currently no analogue of `comparelib` for auto-generation of
hash-functions, so you do need to either write the hash-function by
hand, or use the built-in polymorphic hash function, `Hashtbl.hash`.

## Choosing between maps and hash tables

Maps and hash tables overlap enough in functionality that it's not
always clear when to choose one or the other.  Maps, by virtue of
being immutable, are generally the default choice in OCaml.  OCaml
also has good support for imperative programming, though, and when
programming in an imperative idiom, hash tables are often the more
natural choice.

Programming idioms aside, there are significant performance
differences between maps and hash tables as well.  For code that is
dominated by updates and lookups, hash tables are a clear performance
win, and the win is clearer the larger the size of the tables.

The best way of answering a performance question is by running a
benchmark, so let's do just that.  The following benchmark uses the
`core_bench` library, and it compares maps and hash tables under a very
simple workload.  Here, we're keeping track of a set of 1000 different
integer keys, and cycling over the keys and updating the values they
contain.  Note that we use the `Map.change` and `Hashtbl.change`
functions to update the respective data structures.

```frag
((typ ocaml)(name maps-and-hash-tables/map_vs_hash.ml))
```

The results, shown below, show the hash table version to be around four
times faster than the map version.  


```frag
((typ console)(name maps-and-hash-tables/run_map_vs_hash.out))
```

We can make the speedup smaller or larger depending on the details of
the test; for example, it will vary with the number of distinct keys.
But overall, for code that is heavy on sequences of querying and
updating a set of key/value pairs, hash tables will significantly
outperform maps.

Hash tables are not always the faster choice, though.  In particular,
maps are often more performant in situations where you need to keep
multiple related versions of the data structure in memory at once.  In
particular, if you create map `m'` by calling `Map.add` on some other
map `m`, then `m` and `m'` can be used independently, and in fact
share most of their underlying storage.  Thus, if you need to keep in
memory at the same time multiple different related collections of
key/value pairs, then a map is typically a much more efficient data
structure to do it with.

Here's a benchmark to demonstrates this.  In it, we create a list of
maps (or hash tables) that are built up by iteratively applying
updates, starting from an empty map.  In the hash table implementation,
we do this by calling `Hashtbl.copy` to get the list entries.

```frag
((typ ocaml)(name maps-and-hash-tables/map_vs_hash2.ml))
```

Unsurprisingly, maps perform far better than hash tables on this
benchmark, in this case by more than a factor of ten.  

```frag
((typ console)(name maps-and-hash-tables/run_map_vs_hash2.out))
```

These numbers can be made more extreme by increasing the size of the
tables or the length of the list.

As you can see, the relative performance of trees and maps depends a
great deal on the details of how they're used, and so whether to
choose one data structure or the other will depend on the details of
the application.
