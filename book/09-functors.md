# Functors {#functors data-type=chapter}

Up until now, we've seen OCaml's modules play an important but limited role.
In particular, we've seen them as a mechanism for organizing code into units
with specified interfaces. But OCaml's module system can do much more than
that, serving as a powerful tool for building generic code and structuring
large-scale systems. Much of that power comes from functors.
[functors/benefits of]{.idx}

Functors are, roughly speaking, functions from modules to modules, and they
can be used to solve a variety of code-structuring problems, including:

Dependency injection
: Makes the implementations of some components of a system swappable. This is
  particularly useful when you want to mock up parts of your system for
  testing and simulation purposes.

Autoextension of modules
: Functors give you a way of extending existing modules with new
  functionality in a standardized way. For example, you might want to add a
  slew of comparison operators derived from a base comparison function. To do
  this by hand would require a lot of repetitive code for each type, but
  functors let you write this logic just once and apply it to many different
  types.

Instantiating modules with state
: Modules can contain mutable states, and that means that you'll occasionally
  want to have multiple instantiations of a particular module, each with its
  own separate and independent mutable state. Functors let you automate the
  construction of such modules.

These are really just some of the uses that you can put functors to. We'll
make no attempt to provide examples of all of the uses of functors here.
Instead, this chapter will try to provide examples that illuminate the
language features and design patterns that you need to master in order to use
functors effectively.

## A Trivial Example {#a-trivial-example data-type=sect1}

Let's create a functor that takes a module containing a single integer
variable `x` and returns a new module with `x` incremented by one. This is
intended to serve as a way to walk through the basic mechanics of functors,
even though it's not something you'd want to do in practice. [functors/basic
mechanics of]{.idx}

First, let's define a signature for a module that contains a single value of
type `int`:

<link rel="import" href="code/functors/main.mlt" part="0.5" />

Now we can define our functor. We'll use `X_int` both to constrain the
argument to the functor and to constrain the module returned by the functor:

<link rel="import" href="code/functors/main.mlt" part="1" />

One thing that immediately jumps out is that functors are more syntactically
heavyweight than ordinary functions. For one thing, functors require explicit
(module) type annotations, which ordinary functions do not. Technically, only
the type on the input is mandatory, although in practice, you should usually
constrain the module returned by the functor, just as you should use an
`mli`, even though it's not mandatory.

The following shows what happens when we omit the module type for the output
of the functor:

<link rel="import" href="code/functors/main.mlt" part="2" />

We can see that the inferred module type of the output is now written out
explicitly, rather than being a reference to the named signature `X_int`.

We can use `Increment` to define new modules:

<link rel="import" href="code/functors/main.mlt" part="3" />

In this case, we applied `Increment` to a module whose signature is exactly
equal to `X_int`. But we can apply `Increment` to any module that *satisfies*
the interface `X_int`, in the same way that the contents of an `ml` file must
satisfy the `mli`. That means that the module type can omit some information
available in the module, either by dropping fields or by leaving some fields
abstract. Here's an example:

<link rel="import" href="code/functors/main.mlt" part="4" />

The rules for determining whether a module matches a given signature are
similar in spirit to the rules in an object-oriented language that determine
whether an object satisfies a given interface. As in an object-oriented
context, the extra information that doesn't match the signature you're
looking for (in this case, the variable `y`) is simply ignored.

## A Bigger Example: Computing with Intervals {#a-bigger-example-computing-with-intervals data-type=sect1}

Let's consider a more realistic example of how to use functors: a library for
computing with intervals. Intervals are a common computational object, and
they come up in different contexts and for different types. You might need to
work with intervals of floating-point values or strings or times, and in each
of these cases, you want similar operations: testing for emptiness, checking
for containment, intersecting intervals, and so on.

Let's see how to use functors to build a generic interval library that can be
used with any type that supports a total ordering on the underlying set over
which you want to build intervals. [interval computation/generic library
for]{.idx}[functors/interval computation with]{.idx #FUNCTinterv}

First we'll define a module type that captures the information we'll need
about the endpoints of the intervals. This interface, which we'll call
`Comparable`, contains just two things: a comparison function and the type of
the values to be compared:

<link rel="import" href="code/functors/main.mlt" part="5" />

The comparison function follows the standard OCaml idiom for such functions,
returning `0` if the two elements are equal, a positive number if the first
element is larger than the second, and a negative number if the first element
is smaller than the second. Thus, we could rewrite the standard comparison
functions on top of `compare`.

<link rel="import" href="code/functors/compare_example.ml" />

(This idiom is a bit of a historical error. It would be better if `compare`
returned a variant with three cases for less than, greater than, and equal.
But it's a well-established idiom at this point, and unlikely to change.)

The functor for creating the interval module follows. We represent an
interval with a variant type, which is either `Empty` or `Interval (x,y)`,
where `x` and `y` are the bounds of the interval. In addition to the type,
the body of the functor contains implementations of a number of useful
primitives for interacting with intervals:

<link rel="import" href="code/functors/main.mlt" part="6" />

We can instantiate the functor by applying it to a module with the right
signature. In the following code, rather than name the module first and then
call the functor, we provide the functor input as an anonymous module:

<link rel="import" href="code/functors/main.mlt" part="7" />

If the input interface for your functor is aligned with the standards of the
libraries you use, then you don't need to construct a custom module to feed
to the functor. In this case, we can directly use the `Int` or `String`
modules provided by `Base`:

<link rel="import" href="code/functors/main.mlt" part="8" />

This works because many modules in Base, including `Int` and `String`,
satisfy an extended version of the `Comparable` signature described
previously. Such standardized signatures are good practice, both because they
make functors easier to use, and because they encourage standardization that
makes your codebase easier to navigate.

We can use the newly defined `Int_interval` module like any ordinary module:

<link rel="import" href="code/functors/main.mlt" part="9" />

This design gives us the freedom to use any comparison function we want for
comparing the endpoints. We could, for example, create a type of integer
interval with the order of the comparison reversed, as follows:[interval
computation/comparison function for]{.idx}

<link rel="import" href="code/functors/main.mlt" part="10" />

The behavior of `Rev_int_interval` is of course different from
`Int_interval`:

<link rel="import" href="code/functors/main.mlt" part="11" />

Importantly, `Rev_int_interval.t` is a different type than `Int_interval.t`,
even though its physical representation is the same. Indeed, the type system
will prevent us from confusing them.

<link rel="import" href="code/functors/main.mlt" part="12" />

This is important, because confusing the two kinds of intervals would be a
semantic error, and it's an easy one to make. The ability of functors to mint
new types is a useful trick that comes up a lot.

### Making the Functor Abstract {#making-the-functor-abstract data-type=sect2}

There's a problem with `Make_interval`. The code we wrote depends on the
invariant that the upper bound of an interval is greater than its lower
bound, but that invariant can be violated. The invariant is enforced by the
`create` function, but because `Interval.t` is not abstract, we can bypass
the `create` function:[interval computation/abstract functor for]{.idx}

<link rel="import" href="code/functors/main.mlt" part="13" />

To make `Int_interval.t` abstract, we need to restrict the output of
`Make_interval` with an interface. Here's an explicit interface that we can
use for that purpose:

<link rel="import" href="code/functors/main.mlt" part="14" />

This interface includes the type `endpoint` to give us a way of referring to
the endpoint type. Given this interface, we can redo our definition of
`Make_interval`. Notice that we added the type `endpoint` to the
implementation of the module to match `Interval_intf`:

<link rel="import" href="code/functors/main.mlt" part="15" />

### Sharing Constraints {#sharing-constraints data-type=sect2}

The resulting module is abstract, but it's unfortunately too abstract. In
particular, we haven't exposed the type `endpoint`, which means that we can't
even construct an interval anymore: [sharing constraint]{.idx}

<link rel="import" href="code/functors/main.mlt" part="16" />

To fix this, we need to expose the fact that `endpoint` is equal to `Int.t`
(or more generally, `Endpoint.t`, where `Endpoint` is the argument to the
functor). One way of doing this is through a *sharing constraint*, which
allows you to tell the compiler to expose the fact that a given type is equal
to some other type. The syntax for a simple sharing constraint is as follows:

<link rel="import" href="code/functors/sharing_constraint.syntax" />

The result of this expression is a new signature that's been modified so that
it exposes the fact that *`type`* defined inside of the module type is equal
to *`type'`* whose definition is outside of it. One can also apply multiple
sharing constraints to the same signature:

<link rel="import" href="code/functors/multi_sharing_constraint.syntax" />

We can use a sharing constraint to create a specialized version of
`Interval_intf` for integer intervals:

<link rel="import" href="code/functors/main.mlt" part="17" />

We can also use sharing constraints in the context of a functor. The most
common use case is where you want to expose that some of the types of the
module being generated by the functor are related to the types in the module
fed to the functor.

In this case, we'd like to expose an equality between the type `endpoint` in
the new module and the type `Endpoint.t`, from the module `Endpoint` that is
the functor argument. We can do this as follows:

<link rel="import" href="code/functors/main.mlt" part="18" />

So now, the interface is as it was, except that `endpoint` is known to be
equal to `Endpoint.t`. As a result of that type equality, we can again do
things that require that `endpoint` be exposed, like constructing intervals:

<link rel="import" href="code/functors/main.mlt" part="19" />

### Destructive Substitution {#destructive-substitution data-type=sect2}

Sharing constraints basically do the job, but they have some downsides. In
particular, we've now been stuck with the useless type declaration of
`endpoint` that clutters up both the interface and the implementation. A
better solution would be to modify the `Interval_intf` signature by replacing
`endpoint` with `Endpoint.t` everywhere it shows up, and deleting the
definition of `endpoint` from the signature. We can do just this using what's
called *destructive substitution*. Here's the basic syntax:[destructive
substitution]{.idx}[interval computation/destructive substitution]{.idx}

<link rel="import" href="code/functors/destructive_sub.syntax" />

The following shows how we could use this with `Make_interval`:

<link rel="import" href="code/functors/main.mlt" part="20" />

There's now no `endpoint` type: all of its occurrences of have been replaced
by `int`. As with sharing constraints, we can also use this in the context of
a functor:

<link rel="import" href="code/functors/main.mlt" part="21" />

The interface is precisely what we want: the type `t` is abstract, and the
type of the endpoint is exposed; so we can create values of type
`Int_interval.t` using the creation function, but not directly using the
constructors and thereby violating the invariants of the module:

<link rel="import" href="code/functors/main.mlt" part="22" />

In addition, the `endpoint` type is gone from the interface, meaning we no
longer need to define the `endpoint` type alias in the body of the module.

It's worth noting that the name is somewhat misleading, in that there's
nothing destructive about destructive substitution; it's really just a way of
creating a new signature by transforming an existing one.

### Using Multiple Interfaces {#using-multiple-interfaces data-type=sect2}

Another feature that we might want for our interval module is the ability to
*serialize*, i.e., to be able to read and write intervals as a stream of
bytes. In this case, we'll do this by converting to and from s-expressions,
which were mentioned already in
[Error Handling](07-error-handling.html#error-handling){data-type=xref}. To
recall, an s-expression is essentially a parenthesized expression whose atoms
are strings, and it is a serialization format that is used commonly in
`Base`. Here's an example: [s-expressions/example of]{.idx}[interval
computation/multiple interfaces and]{.idx}

<link rel="import" href="code/functors/main.mlt" part="23" />

`Base` comes with a syntax extension called `ppx_sexp_conv` which will
generate s-expression conversion functions for any type annotated with
`[@@deriving sexp]`. Thus, we can write: [sexp declaration]{.idx}

<link rel="import" href="code/functors/main.mlt" part="24" />

We'll discuss s-expressions and Sexplib in more detail in
[Data Serialization With S Expressions](17-data-serialization.html#data-serialization-with-s-expressions){data-type=xref},
but for now, let's see what happens if we attach the `[@@deriving sexp]`
declaration to the definition of `t` within the functor:

<link rel="import" href="code/functors/main.mlt" part="25" />

The problem is that `[@@deriving sexp]` adds code for defining the
s-expression converters, and that code assumes that `Endpoint` has the
appropriate sexp-conversion functions for `Endpoint.t`. But all we know about
`Endpoint` is that it satisfies the `Comparable` interface, which doesn't say
anything about s-expressions.

Happily, `Base` comes with a built-in interface for just this purpose called
`Sexpable`, which is defined as follows:

<link rel="import" href="code/functors/sexpable.ml" />

We can modify `Make_interval` to use the `Sexpable` interface, for both its
input and its output. First, let's create an extended version of the
`Interval_intf` interface that includes the functions from the `Sexpable`
interface. We can do this using destructive substitution on the `Sexpable`
interface, to avoid having multiple distinct type `t`'s clashing with each
other:

<link rel="import" href="code/functors/main.mlt" part="26" />

Equivalently, we can define a type `t` within our new module, and apply
destructive substitutions to all of the included interfaces, `Interval_intf`
included, as shown in the following example. This is somewhat cleaner when
combining multiple interfaces, since it correctly reflects that all of the
signatures are being handled equivalently:

<link rel="import" href="code/functors/main.mlt" part="27" />

Now we can write the functor itself. We have been careful to override the
sexp converter here to ensure that the data structure's invariants are still
maintained when reading in from an s-expression:

<link rel="import" href="code/functors/main.mlt" part="28" />

And now, we can use that sexp converter in the ordinary way:

<link rel="import" href="code/functors/main.mlt" part="29" />


## Extending Modules {#extending-modules data-type=sect1}

Another common use of functors is to generate type-specific functionality for
a given module in a standardized way. Let's see how this works in the context
of a functional queue, which is just a functional version of a FIFO
(first-in, first-out) queue. Being functional, operations on the queue return
new queues, rather than modifying the queues that were passed
in.[modules/type-specific functionality in]{.idx}[FIFO (first-in, first-out)
queue]{.idx}[functors/module extension with]{.idx}

Here's a reasonable `mli` for such a module:

<link rel="import" href="code/functors/fqueue.mli" />

The preceding `Fqueue.fold` function requires some explanation. It follows
the same pattern as the `List.fold` function we described in
[Using The List Module Effectively](03-lists-and-patterns.html#using-the-list-module-effectively){data-type=xref}.
Essentially, `Fqueue.fold q ~init ~f` walks over the elements of `q` from
front to back, starting with an accumulator of `init` and using `f` to update
the accumulator value as it walks over the queue, returning the final value
of the accumulator at the end of the computation. `fold` is a quite powerful
operation, as we'll see.

We'll implement `Fqueue` the well known trick of maintaining an input and an
output list so that one can efficiently enqueue on the input list and
efficiently dequeue from the output list. If you attempt to dequeue when the
output list is empty, the input list is reversed and becomes the new output
list. Here's the implementation:

<link rel="import" href="code/functors/fqueue.ml" />

One problem with `Fqueue` is that the interface is quite skeletal. There are
lots of useful helper functions that one might want that aren't there. The
`List` module, by way of contrast, has functions like `List.iter`, which runs
a function on each element; and `List.for_all`, which returns true if and
only if the given predicate evaluates to `true` on every element of the list.
Such helper functions come up for pretty much every container type, and
implementing them over and over is a dull and repetitive affair.

As it happens, many of these helper functions can be derived mechanically
from the `fold` function we already implemented. Rather than write all of
these helper functions by hand for every new container type, we can instead
use a functor to add this functionality to any container that has a `fold`
function.

We'll create a new module, `Foldable`, that automates the process of adding
helper functions to a `fold`-supporting container. As you can see, `Foldable`
contains a module signature `S` which defines the signature that is required
to support folding; and a functor `Extend` that allows one to extend any
module that matches `Foldable.S`:

<link rel="import" href="code/functors/foldable.ml" />

Now we can apply this to `Fqueue`. We can create an interface for an extended
version of `Fqueue` as follows:

<link rel="import" href="code/functors/extended_fqueue.mli" />

In order to apply the functor, we'll put the definition of `Fqueue` in a
submodule called `T`, and then call `Foldable.Extend` on `T`:

<link rel="import" href="code/functors/extended_fqueue.ml" />

`Base` comes with a number of functors for extending modules that follow this
same basic pattern, including:
[Monad.Make]{.idx}[Hashable.Make]{.idx}[Comparable
module/Comparable.Make]{.idx}[Container.Make]{.idx}

`Container.Make`
: Very similar to `Foldable.Extend`.

`Comparable.Make`
: Adds support for functionality that depends on the presence of a comparison
  function, including support for containers like maps and sets.

`Hashable.Make`
: Adds support for hashing-based data structures including hash tables, hash
  sets, and hash heaps.

`Monad.Make`
: For so-called monadic libraries, like those discussed in Chapters
  [Error Handling](07-error-handling.html#error-handling){data-type=xref data-xrefstyle="select:
  labelnumber"} and
  [Concurrent Programming With Async](18-concurrent-programming.html#concurrent-programming-with-async){data-type=xref data-xrefstyle="select:
  labelnumber"}. Here, the functor is used to provide a collection of
  standard helper functions based on the `bind` and `return` operators.

These functors come in handy when you want to add the same kind of
functionality that is commonly available in `Base` to your own types.

We've really only covered some of the possible uses of functors. Functors are
really a quite powerful tool for modularizing your code. The cost is that
functors are syntactically heavyweight compared to the rest of the language,
and that there are some tricky issues you need to understand to use them
effectively, with sharing constraints and destructive substitution being high
on that list.

All of this means that for small and simple programs, heavy use of functors
is probably a mistake. But as your programs get more complicated and you need
more effective modular architectures, functors become a highly valuable tool.


