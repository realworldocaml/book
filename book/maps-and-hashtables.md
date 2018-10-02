# Maps and Hash Tables {#maps-and-hash-tables}

Lots of programming problems require dealing with data organized as key/value
pairs. Maybe the simplest way of representing such data in OCaml is an
*association list*, which is simply a list of pairs of keys and values. For
example, you could represent a mapping between the 10 digits and their
English names as follows: [key/value pairs]{.idx}[data structures/key/value
pairs]{.idx}[lists/association lists]{.idx}[association lists]{.idx}

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="1" />

We can use functions from the `List.Assoc` module to manipulate this data:

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="2" />

Association lists are simple and easy to use, but their performance is not
ideal, since almost every nontrivial operation on an association list
requires a linear-time scan of the list.

In this chapter, we'll talk about two more efficient alternatives to
association lists: *maps* and *hash tables*. A map is an immutable tree-based
data structure where most operations take time logarithmic in the size of the
map, whereas a hash table is a mutable data structure where most operations
have constant time complexity. We'll describe both of these data structures
in detail and provide some advice as to how to choose between them. [hash
tables/basics of]{.idx}[maps/basics of]{.idx}

## Maps {#maps}

Let's consider an example of how one might use a map in practice. In
[Files Modules And Programs](files-modules-and-programs.html#files-modules-and-programs){data-type=xref},
we showed a module `Counter` for keeping frequency counts on a set of
strings. Here's the interface:

<link rel="import" href="code/files-modules-and-programs/freq-fast/counter.mli" />

The intended behavior here is straightforward. `Counter.empty` represents an
empty collection of frequency counts; `touch` increments the frequency count
of the specified string by 1; and `to_list` returns the list of nonzero
frequencies.

Here's the implementation.

<link rel="import" href="code/files-modules-and-programs/freq-fast/counter.ml" />

Take a look at the definition of the type `t` above. You'll see that the
`Map.t` has three type parameter. The first two are what you might expect;
one for the type of the key, and one for type of the data. The third type
parameter, the *comparator witness*, requires some explaining.

The comparator witness is used to indicate which comparison function was used
to construct the map, rather than saying anything about concrete data stored
in the map. The type `String.comparator_witness` in particular indicates that
this map was built with the default comparison function from the `String`
module. We'll talk about why the comparator witness is important later in the
chapter.

The call to `Map.empty` is also worth explaining, in that, unusually, it
takes a first-class module as an argument. The point of the first class
module is to provide the comparison function that is required for building
the map, along with an s-expression converter for generating useful error
messages (we'll talk more about s-expressions in
[Data Serialization with S-Expressions](data-serialization.html){data-type=xref}).
We don't need to provide the module again for functions like `Map.find` or
`Map.add`, because the map itself contains a reference to the comparison
function it uses.

Not every module can be used for creating maps, but the standard ones in
`Base` are. Later in the chapter, we'll show how you can set up a module your
own so it can be used in this way.

### Sets {#sets}

In addition to maps, `Base` also provides a set data type that's designed
along similar lines. In some sense, sets are little more than maps where you
ignore the data. But while you could encode sets in terms of maps, it's more
natural, and more efficient, to use `Base`'s specialized set type. Here's a
simple example. [set types]{.idx}

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="17" />

In addition to the operators you would expect to have for maps, sets support
the traditional set operations, including union, intersection, and set
difference. And, as with maps, we can create sets based on type-specific
comparators or on the polymorphic comparator.

### Modules and Comparators {#modules-and-comparators}

It's easy enough to create a map or set based on a type represented by a
module in `Base`. Here, we'll create a map from digits to their English
names, based on `digit_alist`, which was defined earlier in the chapter.

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="mc.1" />

The function `Map.of_alist_exn` constructs a map from a provided association
list, throwing an exception if a key is used more than once. Let's take a
look at the type signature of `Map.of_alist_exn`.

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="mc.2" />

The type `Map.comparator` is actually an alias for a first-class module type,
representing any module that matches the signature `Comparator.S`, shown
below.

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="mc.2.1" />

Such a module needs to contain the type of the key itself, as well as the
`comparator_witness` type, which serves as a type-level identifier of the
comparison function in question, and finally, the concrete comparator itself,
a value that contains the necessary comparison function.

Modules from `Base` like `Int` and `String` already satisfy this interface.
But what if you want to satisfy this interface with a new module? Consider,
for example, the following type representing a book, for which we've written
a comparison function and an s-expression serializer.

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="mc.2.2" />

This module has the basic functionality we need, but doesn't satisfy the
`Comparator.S` interface, so we can't use it for creating a map, as you can
see.

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="mc.2.3" />

In order to satisfy the interface, we need to use the `Comparator.Make`
functor to extend the module. Here, we use a common idiom where we create a
submodule, called `T` containing the basic functionality for the type in
question, and then include both that module and the result of applying a
functor to that module.

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="mc.2.4" />

With this module in hand, we can now build a set using the type `Book.t`.

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="mc.2.5" />

### Why do we need comparator witnesses? {#why-comparator-witnesses}

The comparator witness looks a little surprising at first, and it may not be
obvious why it's there in the first place. The purpose of the witness is to
identify the comparison function being used. This is important because some
of the operations on maps and sets, in particular those that combine multiple
maps or sets together, depend for their correctness on the fact that the
different maps are using the same comparison function.

Consider, for example, `Map.symmetric_diff`, which computes the difference
between two maps.

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="mc.3" />

The type of `Map.symmetric_diff`, which follows, requires that the two maps
it compares have the same comparator type, and therefore the same comparison
function.

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="mc.4" />

Without this constraint, we could run `Map.symmetric_diff` on maps that are
sorted in different orders, which could lead to garbled results. We can show
how this works in practice by creating two maps with the same key and data
types, but different comparison functions. In the following, we do this by
minting a new module `Reverse`, which represents strings sorted in the
reverse of the usual lexicographic order.

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="mc.5" />

As you can see in the following, both `Reverse` and `String` can be used to
create maps with a key type of `string`:

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="mc.6" />

`Map.min_elt` returns the key and value for the smallest key in the map,
which confirms that these two maps do indeed use different comparison
functions.

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="mc.7" />

As such, running `Map.symmetric_diff` on these maps doesn't make any sense.
Happily, the type system will give us a compile-time error if we try, instead
of throwing an error at run time, or worse, silently returning the wrong
result.

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="mc.8" />

### The Polymorphic Comparator {#the-polymorphic-comparator}

We don't need to generate specialized comparators for every type we want to
build a map on. We can instead build a map based on OCaml's built-in
polymorphic comparison function, which was discussed in
[Lists And Patterns](lists-and-patterns.html#lists-and-patterns){data-type=xref}.
`Base` currently doesn't have a convenient function for minting maps based on
polymorphic compare, but `Core_kernel` does, as we can see below.
[maps/polymorphic comparison in]{.idx}[polymorphic comparisons]{.idx}

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="pc.1" />

Note that maps based on the polymorphic comparator have different comparator
witnesses than those based on the type-specific comparison function. Thus,
the compiler rejects the following:

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="pc.2" />

This is rejected for good reason: there's no guarantee that the comparator
associated with a given type will order things in the same way that
polymorphic compare does.

<aside data-type="sidebar">
<h5>The Perils of Polymorphic Compare</h5>

Polymorphic compare is highly convenient, but it has serious downsides as
well and should be used with care. In particular, polymorphic compare has a
fixed algorithm for comparing values of any type, and that algorithm can
sometimes yield surprising results.

To understand what's wrong with polymorphic compare, you need to understand a
bit about how it works. Polymorphic compare is *structural*, in that it
operates directly on the runtime representation of OCaml values, walking the
structure of the values in question without regard for their type.

This is convenient because it provides a comparison function that works for
most OCaml values and largely behaves as you would expect. For example, on
`int`s and `float`s, it acts as you would expect a numeric comparison
function to act, and for simple containers like strings and lists and arrays,
it operates as a lexicographic comparison. Except for values from outside of
the OCaml heap and functions, it works on almost every OCaml type.

But sometimes, a structural comparison is not what you want. Maps are
actually a fine example of this. Consider the following two maps.

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="ppc.1" />

Logically, these two sets should be equal, and that's the result that you get
if you call `Map.equal` on them:

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="ppc.2" />

But because the elements were added in different orders, the layout of the
trees underlying the sets will be different. As such, a structural comparison
function will conclude that they're different.

Let's see what happens if we use polymorphic compare to test for equality.
`Base` hides polymorphic comparison by defaults, but it is available by
opening the `Poly` module, at which point `=` is bound to polymorphic
equality. Comparing the maps directly will fail at runtime because the
comparators stored within the sets contain function values:

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="ppc.3" />

We can, however, use the function `Map.Using_comparator.to_tree` to expose
the underlying binary tree without the attached comparator. This same issue
comes up with other data types, including sets, which we'll discuss later in
the chapter.

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="ppc.4" />

This can cause real and quite subtle bugs. If, for example, you use a map
whose keys contain sets, then the map built with the polymorphic comparator
will behave incorrectly, separating out keys that should be aggregated
together. Even worse, it will work sometimes and fail others; since if the
sets are built in a consistent order, then they will work as expected, but
once the order changes, the behavior will change.

</aside>

### Satisfying `Comparator.S` with `[@@deriving]` {#satsifying-comparator.s-with-deriving}

Using maps and sets on a new type requires satisfying the `Comparator.S`
interface, which in turn requires s-expression converters and comparison
functions for the type in question. Writing such functions by hand is
annoying and error prone, but there's a better way. `Base` comes along with a
set of syntax extensions that automate these tasks away.

Let's return to an example from earlier in the chapter, where we created a
type `Book.t` and set it up for use in creating maps and sets.

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="mc.2.4" />

Much of the code here is devoted to creating a comparison function and
s-expression converter for the type `Book.t`. But if we have the
ppx_sexp_conv and ppx_compare syntax extensions enabled (both of which come
with the omnibus ppx_jane package), then we can request that default
implementations of these functions be created, as follows.

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="ud.1" />

If you want your comparison function that orders things in a particular way,
you can always write your own comparison function by hand; but if all you
need is a total order suitable for creating maps and sets with, then
`[@@deriving compare]` is a good choice.

<aside data-type="sidebar">
<h5>=, ==, and phys_equal</h5>

If you come from a C/C++ background, you'll probably reflexively use 
`==` to test two values for equality. In OCaml, the `==` operator tests for
*physical* equality, while the `=` operator tests for *structural* equality.

The physical equality test will match if two data structures have precisely
the same pointer in memory. Two data structures that have identical contents
but are constructed separately will not match using `==`.

The `=` structural equality operator recursively inspects each field in the
two values and tests them individually for equality. Crucially, if your data
structure is cyclical (that is, a value recursively points back to another
field within the same structure), the `=` operator will never terminate, and
your program will hang! You therefore must use the physical equality operator
or write a custom comparison function when comparing cyclic values.

It's quite easy to mix up the use of `=` and `==`, so Core_kernel discourages
the use of `==` and provides the more explicit `phys_equal` function instead.
You'll see a warning if you use `==` anywhere in code that opens
`Core_kernel`:

<link rel="import" href="code/maps-and-hash-tables/core_phys_equal.mlt" part=
"1" />

If you feel like hanging your OCaml interpreter, you can verify what happens
with recursive values and structural equality for yourself:

<link rel="import" href="code/maps-and-hash-tables/phys_equal.rawscript" />

</aside>

### Applying `[@@deriving]` to maps and sets {#applying-deriving-to-maps-and-sets}

In the previous section, we showed how to use `[@@deriving]` annotations to
set up a type so it could be used to create a map or set type. But what if we
want to put a `[@@deriving]` annotation on a map or set type itself?

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="adm.1" />

This fails because there is no existing `Map.t_of_sexp`. This isn't a simple
omission; there's no reasonable way to define a useful `Map.t_of_sexp`,
because a comparator witness isn't something that can be parsed out of the
s-expression.

Happily, there's another way of writing the type of a map that does work with
the various `[@@deriving]` extensions, which you can see below.

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="adm.2" />

Here, we use a functor, `Map.M`, to define the type we need. While this looks
different than the ordinary type signature, the meaning of the type is the
same, as we can see below.

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="adm.3" />

This same type works well with other derivers, like those for comparison and
hash functions. Since this way of writing the type is also shorter, it's what
you should use most of the time.

### Trees {#trees}

As we've discussed, maps carry within them the comparator that they were
created with. Sometimes, for space efficiency reasons, you want a version of
the map data structure that doesn't include the comparator. You can get such
a representation with `Map.Using_comparator.to_tree`, which returns just the
tree underlying the map, without the comparator. [Map
module/Map.to_tree]{.idx}[maps/tree structure]{.idx}

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="11" />

Even though the tree doesn't physically include a comparator, it does include
the comparator in its type. This is what is known as a *phantom type*,
because it reflects something about the logic of the value in question, even
though it doesn't correspond to any values directly represented in the
underlying physical structure of the value.

Since the comparator isn't included in the tree, we need to provide the
comparator explicitly when we, say, search for a key, as shown below:

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="12" />

The algorithm of `Map.Tree.find` depends on the fact that it's using the same
comparator when looking up a value as you were when you stored it. That's the
invariant that the phantom type is there to enforce. As you can see in the
following example, using the wrong comparator will lead to a type error:

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="13" />


## Hash Tables {#hash-tables}

Hash tables are the imperative cousin of maps. We walked over a basic hash
table implementation in
[Imperative Programming 1](imperative-programming.html#imperative-programming-1){data-type=xref},
so in this section we'll mostly discuss the pragmatics of Core's `Hashtbl`
module. We'll cover this material more briefly than we did with maps because
many of the concepts are shared. [hash tables/basics of]{.idx}

Hash tables differ from maps in a few key ways. First, hash tables are
mutable, meaning that adding a key/value pair to a hash table modifies the
table, rather than creating a new table with the binding added. Second, hash
tables generally have better time-complexity than maps, providing
constant-time lookup and modifications, as opposed to logarithmic for maps.
And finally, just as maps depend on having a comparison function for creating
the ordered binary tree that underlies a map, hash tables depend on having a
*hash function*, i.e., a function for converting a key to an integer.
[functions/hash functions]{.idx}[Hashtbl module]{.idx}[hash tables/time
complexity of]{.idx}

::: {.allow_break data-type=warning}
### Time Complexity of Hash Tables

The statement that hash tables provide constant-time access hides some
complexities. First of all, any hash table implementation, OCaml's included,
needs to resize the table when it gets too full. A resize requires allocating
a new backing array for the hash table and copying over all entries, and so
it is quite an expensive operation. That means adding a new element to the
table is only *amortized* constant, which is to say, it's constant on average
over a long sequence of operations, but some of the individual operations can
cost more.

Another hidden cost of hash tables has to do with the hash function you use.
If you end up with a pathologically bad hash function that hashes all of your
data to the same number, then all of your insertions will hash to the same
underlying bucket, meaning you no longer get constant-time access at all.
`Base`'s hash table implementation uses binary trees for the hash-buckets, so
this case only leads to logarithmic time, rather than linear for a
traditional implementation.

The logarithmic behavior of Base's hash tables in the presence of hash
collisions also helps protect against some denial-of-service attacks. One
well-known type of attack is to send queries to a service with carefully
chosen keys to cause many collisions. This, in combination with the linear
behavior of most hashtables, can cause the service to become unresponsive due
to high CPU load. Base's hash tables would be much less susceptible to such
an attack because the amount of degradation would be far less. [security
issues/denial-of-service attacks]{.idx}[denial-of-service attacks,
avoiding]{.idx}
:::


We create a hashtable in a way that's similar to how we create maps, by
providing a first-class module from which the required operations for
building a hashtable can be obtained.

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="ht.1" />

As with maps, most modules in Base are ready to be used for this purpose, but
if you want to create a hash table from one of your own types, you need to do
some work to prepare it. In order for a module to be suitable for passing to
`Hashtbl.create`, it has to match the following interface.

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="ht.2" />

Note that there's no equivalent to the comparator witness that came up for
maps and sets. That's because the requirement for multiple objects to share a
comparison function or a hash function mostly just doesn't come up for hash
tables. That makes building a module suitable for use with a hash table
simpler.

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="ht.3" />

You can also create a hashtable based on OCaml's polymorphic hash and
comparison functions.

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="ht.4" />

This is highly convenient, but polymorphic comparison can behave in
surprising ways, so it's generally best to avoid this for code where
correctness matters.

::: {data-type=warning}
### Collisions with the Polymorphic Hash Function

The polymorphic hash function, like polymorphic compare, has problems that
derive from the fact that it doesn't pay any attention to the type, just
blindly walking down the structure of a data type and computing a hash from
what it sees. That means that for data structures like maps and sets where
equivalent instances can have different structures, it will do the wrong
thing.

But there's another problem with polymorphic hash, which is that it is prone
to creating hash collisions. OCaml's polymorphic hash function works by
walking over the data structure it’s given using a breadth-first traversal
that is bounded in the number of nodes it’s willing to traverse. By
default, that bound is set at 10 "meaningful" nodes. [hash tables/polymorphic
hash function]{.idx}

The bound on the traversal means that the hash function may ignore part of
the data structure, and this can lead to pathological
<span class="keep-together">cases</span> where every value you store has the
same hash value. We'll demonstrate this below, using the function
`List.range` to allocate lists of integers of different length:

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="ph.1" />

As you can see, the hash function stops after the first 10 elements. The same
can happen with any large data structure, including records and arrays. When
building hash functions over large custom data structures, it is generally a
good idea to write one's own hash function, or to use the ones provided by
`[@@deriving]`, which don't have this problem, as you can see below.

<link rel="import" href="code/maps-and-hash-tables/main.mlt" part="ph.2" />

Note that rather than declaring a type and using `[@@deriving hash]` to
invoke ppx_hash, we use `[%%hash]`, a shorthand for creating a hash function
inline in an expression.
:::


## Choosing Between Maps and Hash Tables {#choosing-between-maps-and-hash-tables}

Maps and hash tables overlap enough in functionality that it's not always
clear when to choose one or the other. Maps, by virtue of being immutable,
are generally the default choice in OCaml. OCaml also has good support for
imperative programming, though, and when programming in an imperative idiom,
hash tables are often the more natural choice. [maps/vs.
hashtables]{.idx}[hash tables/vs. maps]{.idx}

Programming idioms aside, there are significant performance differences
between maps and hash tables. For code that is dominated by updates and
lookups, hash tables are a clear performance win, and the win is clearer the
larger the amount of data.

The best way of answering a performance question is by running a benchmark,
so let's do just that. The following benchmark uses the `core_bench` library,
and it compares maps and hash tables under a very simple workload. Here,
we're keeping track of a set of 1,000 different integer keys and cycling over
the keys and updating the values they contain. Note that we use the
`Map.change` and `Hashtbl.change` functions to update the respective data
structures:

<link rel="import" href="code/maps-and-hash-tables/map_vs_hash/map_vs_hash.ml" />

The results show the hash table version to be around four times faster than
the map version:

<link rel="import" href="code/maps-and-hash-tables/map_vs_hash/jbuild" />

<link rel="import" href="code/maps-and-hash-tables/map_vs_hash/run_map_vs_hash.sh" />

We can make the speedup smaller or larger depending on the details of the
test; for example, it will vary with the number of distinct keys. But
overall, for code that is heavy on sequences of querying and updating a set
of key/value pairs, hash tables will significantly outperform maps.

Hash tables are not always the faster choice, though. In particular, maps
excel in situations where you need to keep multiple related versions of the
data structure in memory at once. That's because maps are immutable, and so
operations like `Map.add` that modify a map do so by creating a new map,
leaving the original undisturbed. Moreover, the new and old maps share most
of their physical structure, so multiple versions can be kept around
efficiently.

Here's a benchmark that demonstrates this. In it, we create a list of maps
(or hash tables) that are built up by iteratively applying small updates,
keeping these copies around. In the map case, this is done by using
`Map.change` to update the map. In the hash table implementation, the updates
are done using `Hashtbl.change`, but we also need to call `Hashtbl.copy` to
take snapshots of the table:

<link rel="import" href="code/maps-and-hash-tables/map_vs_hash2/map_vs_hash2.ml" />

Unsurprisingly, maps perform far better than hash tables on this benchmark,
in this case by more than a factor of 10:

<link rel="import" href="code/maps-and-hash-tables/map_vs_hash2/jbuild" />

<link rel="import" href="code/maps-and-hash-tables/map_vs_hash2/run_map_vs_hash2.sh" />

These numbers can be made more extreme by increasing the size of the tables
or the length of the list.

As you can see, the relative performance of trees and maps depends a great
deal on the details of how they're used, and so whether to choose one data
structure or the other will depend on the details of the application.
[phys_equal function]{.idx}[equal equal (= =) operator]{.idx}[equal (=)
operator]{.idx}[structural equality]{.idx}[physical equality]{.idx}[equality,
tests of]{.idx}


