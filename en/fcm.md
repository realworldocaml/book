# First-class modules

You can think of OCaml as being broken up into two parts: a core
language that is concerned with values and types, and a module
language that is concerned with modules and module signatures.  These
sub-languages are stratified, in that modules can contain types and
values, but ordinary values can't contain modules or module types.
That means you can't do things like define a variable whose value is a
module, or a function that takes a module as an argument.

OCaml provides a way around this stratification in the form of
_first-class modules_.  First-class modules are ordinary values that
can be created from and converted back to regular modules.  

First-class modules are a sophisticated technique, and you'll need to
get comfortable with some advanced aspects of the language to use them
effectively.  But it's worth learning, because letting modules into
the core language is quite powerful, increasing the range of what you
can express and making it easier to build flexible and modular
systems.

## Working with first-class modules

We'll start out by covering the basic mechanics of first-class modules
by working through some toy examples.  We'll get to more realistic
examples in the next section.

In that light, consider the following signature of a module with a
single integer variable.

```frag
((typ ocamltop)(name fcm/main.topscript)(part 0))
```

We can also create a module that matches this signature.

```frag
((typ ocamltop)(name fcm/main.topscript)(part 1))
```

A first-class module is created by packaging up a module with a
signature that it satisfies.  This is done using the `module` keyword,
using the following syntax:

```frag
((typ ocamlsyntax)(name fcm/pack.syntax))
```

So, we can convert `Three` into a first-class module as follows.

```frag
((typ ocamltop)(name fcm/main.topscript)(part 2))
```

The module type doesn't need to be part of the construction of a
first-class module if it can be inferred.  Thus, we can write:

```frag
((typ ocamltop)(name fcm/main.topscript)(part 3))
```

We can also create a first-class module from an anonymous module:

```frag
((typ ocamltop)(name fcm/main.topscript)(part 4))
```

In order to access the contents of a first-class module, you need to
unpack it into an ordinary module.  This can be done using the `val`
keyword, using this syntax:

```frag
((typ ocamlsyntax)(name fcm/unpack.syntax))
```

And here's an example.

```frag
((typ ocamltop)(name fcm/main.topscript)(part 5))
```

<note> <title> Equality of first-class module types </title>

The type of the first-class module, _e.g._, `(module X_int)`, is based
on the fully-qualified name of the signature that was used to
construct it.  A first-class module based on a signature with a
different name, even if it is substantively the same signature, will
result in a distinct type, as you can see below.

```frag
((typ ocamltop)(name fcm/main.topscript)(part 6))
```

Even though their types as first-class modules are distinct, the
underlying module types are compatible (indeed, identical), so we can
unify the types by unpacking and repacking the module.

```frag
((typ ocamltop)(name fcm/main.topscript)(part 7))
```

The way in which type equality for first-class modules is determined
can be confusing.  One common and problematic case is that of creating
an alias of a module type defined elsewhere.  This is often done to
improve readability, and can happen both through an explicit
declaration of a module type or implicitly through an `include`
declaration.  In both cases, this has the unintended side effect of
making first-class modules built off of the alias incompatible with
those built off of the original module type.  To deal with this, one
should be disciplined in how one refers to signatures when
constructing first-class modules.

</note>

We can also write ordinary functions which consume and create first
class modules.  The following shows the definition of two functions:
`to_int`, which converts a `(module X_int)` into an `int`; and `plus`,
which returns the sum of two `(module X_int)`s.

```frag
((typ ocamltop)(name fcm/main.topscript)(part 8))
```

With these functions in hand, we can now work with values of type
`(module X_int)` in a more natural style, taking advantage of the
concision and simplicity of the core language.

```frag
((typ ocamltop)(name fcm/main.topscript)(part 9))
```

There are some useful syntactic shortcuts when dealing with first
class modules.  One notable one is that you can do the conversion to an
ordinary module within a pattern match.  Thus, we can rewrite the
`to_int` function as follows.

```frag
((typ ocamltop)(name fcm/main.topscript)(part 10))
```

First-class modules can contain types and functions in addition to
simple values like `int`.  Here's an interface that contains a type
and a corresponding `bump` operation that takes a value of the type
and produces a new one.

```frag
((typ ocamltop)(name fcm/main.topscript)(part 11))
```

We can create multiple instances of this module with different
undelrying types.

```frag
((typ ocamltop)(name fcm/main.topscript)(part 12))
```

And we can convert these to first-class modules.

```frag
((typ ocamltop)(name fcm/main.topscript)(part 13))
```

But you can't do much with `int_bumper`, since `int_bumper` is fully
abstract, so that we can no longer recover the fact that the type in
question is `int`.  This means you can't really do much with it, as
you can see below.

```frag
((typ ocamltop)(name fcm/main.topscript)(part 14))
```

To make `int_bumber` usable, we need to expose the type, which we can
do as follows.

```frag
((typ ocamltop)(name fcm/main.topscript)(part 15))
```

The sharing constraints we've added above make the resulting
first-class modules polymorphic in the type `t`.  As a result, we can
now use these values on values of the matching type.

```frag
((typ ocamltop)(name fcm/main.topscript)(part 16))
```

We can also write functions that use such first-class modules
polymorphically.  The following function takes two arguments: a
`Bumpable` module, and a list of elements of the same type as the type
`t` of the module.

```frag
((typ ocamltop)(name fcm/main.topscript)(part 17))
```

Here, we used a feature of OCaml that hasn't come up before: a
_locally abstract type_.  For any function, you can declare a
pseudo-parameter of the form `(type a)` for any type name `a` which
introduces a fresh type that acts like an abstract type within the
context of the function.  Here, we used that type as part of a sharing
constraint that ties the type `B.t` with the type of the elements of
the list passed in.

The resulting function is polymorphic in both the type of the list
element and the type `Bumpable.t`.  We can see this function in action
below.

```frag
((typ ocamltop)(name fcm/main.topscript)(part 18))
```

Polymorphic first-class modules are important because they allow you
to connect the types associated with a first-class module to the types
of other values you're working with.

<note><title> More on locally abstract types </title>

One of the key properties of locally abstract types is that they are
dealt with as abstract types within the function they're defined
within, but are polymorphic from the outside.  Consider the following
example.

```frag
((typ ocamltop)(name fcm/main.topscript)(part 19))
```

This compiles successfully because the type `a` is used in a way that
is compatible with it being abstract, but the type of the function
that is inferred is polymorphic.

If, on the other hand, we try to use the type `a` as equivalent to
some concrete type, say, `int`, then the compiler will complain.

```frag
((typ ocamltop)(name fcm/main.topscript)(part 20))
```

One common use of locally abstract types is to create a new type that
can be used in constructing a module.  Here's an example of doing this
to create a new first-class module.

```frag
((typ ocamltop)(name fcm/main.topscript)(part 21))
```

Here, what we effectively do is capture a polymorphic type and export
it as a concrete type within a module.

This technique is useful beyond first-class modules.  For example, we
can use the same approach to construct a local module to be fed to a
functor.

</note>

## Example: A query handling framework

Now let's look at first-class modules in the context of a more
complete and realistic example.  In particular, consider the following
signature for a module that implements a query handler.

```frag
((typ ocamltop)(name fcm/query_handler.topscript)(part 0))
```

In the above we use s-expressions as the format for queries and
responses, as well for the config.  S-expressions are a simple,
flexible, and human-readable serialization format commonly used in
Core.  We'll cover s-expressions in more detail in
[xref](#data-serialization-with-s-expressions), but for now, it's
enough to think of them as balanced parenthetical expressions whose
atomic values are strings, _e.g._, `(this (is an) (s expression))`.

In addition, we use the `sexplib` syntax extension which extends OCaml
by adding the `with sexp` declaration.  When attached to a type in a
signature, `with sexp` adds declarations of s-expression converters,
_e.g._,

```frag
((typ ocamltop)(name fcm/query_handler.topscript)(part 1))
```

In a module, `with sexp` adds the implementation of those functions.
Thus, we can write

```frag
((typ ocamltop)(name fcm/query_handler.topscript)(part 2))
```

This is all described in more detail in
[xref](#data-serialization-with-s-expressions).

### Implementing a query handler

Let's look at some examples of query handlers that satisfy this
interface.  The following handler produces unique integer ids by
keeping an internal counter which it bumps every time it produces a
new value.  The input to the query in this case is just the trivial
s-expression `()`, otherwise known as `Sexp.unit`.

```frag
((typ ocamltop)(name fcm/query_handler.topscript)(part 3))
```

We can use this module to create an instance of the `Unique` query
handler and interact with it.

```frag
((typ ocamltop)(name fcm/query_handler.topscript)(part 4))
```

Here's another example: a query handler that does directory listings.
Here, the config is the default directory that relative paths are
interpreted within.

```frag
((typ ocamltop)(name fcm/query_handler.topscript)(part 5))
```

Again, we can create an instance of this query handler and interact
with it directly.

```frag
((typ ocamltop)(name fcm/query_handler.topscript)(part 6))
```

### Dispatching to multiple query handlers

Now, what if we want to dispatch queries to any of an arbitrary
collection of handlers?  Ideally, we'd just like to pass in the
handlers as a simple data structure like a list.  This is awkward to
do with modules and functors alone, but it's quite natural with
first-class modules.  The first thing we'll need to do is create a
signature that combines a `Query_handler` module with an instantiated
query handler.

```frag
((typ ocamltop)(name fcm/query_handler.topscript)(part 7))
```

With this signature, we can create a first-class module that
encompasses both an instance of the query and the matching operations
for working with that query.

We can create an instance as follows.

```frag
((typ ocamltop)(name fcm/query_handler.topscript)(part 8))
```

Constructing instances in this way is a little verbose, but we can
write a function that eliminates most of this boilerplate.  Note that
we are again making use of a locally abstract type.

```frag
((typ ocamltop)(name fcm/query_handler.topscript)(part 9))
```

Using `build_instance`, constructing a new instance becomes a
one-liner:

```frag
((typ ocamltop)(name fcm/query_handler.topscript)(part 10))
```

The following code lets you dispatch queries to one of a list of query
handler instances.  We assume that the shape of the query is as
follows:

```frag
((typ scheme)(name fcm/query-syntax.scm))
```

where `query-name` is the name used to determine which query handler
to dispatch the query to, and `query` is the body of the query.

The first thing we'll need is a function that takes a list of query
handler instances and constructs a dispatch table from it.

```frag
((typ ocamltop)(name fcm/query_handler.topscript)(part 11))
```

Now, we need a function that dispatches to a handler using a dispatch
table.

```frag
((typ ocamltop)(name fcm/query_handler.topscript)(part 12))
```

This function interacts with an instance by unpacking it into a module
`I` and then using the query handler instance (`I.this`) in concert
with the associated module (`I.Query_handler`).

The bundling together of the module and the value is in many ways
reminiscent of object-oriented languages.  One key difference, is
that first-class modules allow you to package up more than just a
functions or methods.  As we've seen, you can also include types and
even modules.  We've only used it in a small way here, but this extra
power allows you to build more sophisticated components that involve
multiple interdependent types and values.

Now let's turn this into a complete, running example, by adding a
command-line interface, as shown below.

```frag
((typ ocaml)(name fcm/query_handler.topscript)(part 13))
```

We can most effectively run this command-line interface from a
standalone program, which we can do by putting the above code in a
file along with following command to launch the interface.

```frag
((typ ocaml)(name fcm/query_handler.ml)(part 1))
```

Here's an example of a session with this program.

```frag
((typ console)(name fcm/query_example.rawscript))
```

### Loading and unloading query handlers

One of the advantages of first-class modules is that they afford a
great deal of dynamism and flexibility.  For example, it's a fairly
simple matter to change our design to allow query handlers to be
loaded and unloaded at runtime.  

We'll do this by creating a query handler whose job is to control the
set of active query handlers.  The module in question will be called
`Loader`, and its configuration is a list of known `Query_handler`
modules.  Here are the basic types.

```frag
((typ ocaml)(name fcm/query_handler_core.ml)(part 1))
```

Note that a `Loader.t` has two hash tables: one containing the known
query handler modules, and one containing the active query handler
instances.  The `Loader.t` will be responsible for creating new
instances and adding them to the table, as well as for removing
instances, all in response to user queries.

Next, we'll need a function for creating a `Loader.t`.  This function
requires the list of known query handler modules.  Note that the table
of active modules starts out as empty.

```frag
((typ ocaml)(name fcm/query_handler_core.ml)(part 2))
```

Now we'll start writing out the functions for manipulating the table
of active query handlers.  We'll start with the function for loading
an instance.  Note that it takes as an argument both the name of the
query handler, and the configuration for instantiating that handler,
in the form of an s-expression.  These are used for creating a
first-class module of type `(module Query_handler_instance)`, which is
then added to the active table.  

```frag
((typ ocaml)(name fcm/query_handler_core.ml)(part 3))
```

Since the `load` function will refuse to `load` an already active
handler, we also need the ability to unload a handler.  Note that the
handler explicitly refuses to unload itself.


```frag
((typ ocaml)(name fcm/query_handler_core.ml)(part 4))
```

Finally, we need to implement the `eval` function, which will
determine the query interface presented to the user.  We'll do this by
creating a variant type, and using the s-expression converter
generated for that type to parse the query from the user.

```frag
((typ ocaml)(name fcm/query_handler_core.ml)(part 5))
```

The eval function itself is fairly straight-forward, dispatching to
the appropriate functions to respond to each type of query.  Note that
we use write `<sexp_of<string list>>` to autogenerate a function for
converting a list of strings to an s-expression.  This is part of the
sexplib package described in
[xref](#data-serialization-with-s-expressions).  

This function ends the definition of the `Loader` module.

```frag
((typ ocaml)(name fcm/query_handler_core.ml)(part 6))
```

Finally, we can put this all together with the command line interface.
We first create an instance of the loader query handler, and then add
that instance to the loader's active table.  We can then just launch
the command-line interface, passing it the active table.

```frag
((typ ocaml)(name fcm/query_handler_loader.ml)(part 1))
```

Now link this into a command line interface to experiment with it.

```frag
((typ console)(name fcm/build_query_handler_loader.out))
```

The resulting command line interface behaves much as you'd expect,
starting out with no query handlers available, but giving you the
ability to load and unload them.  Here's an example of it in action.
As you can see, we start out with `loader` itself as the only active
handler.

```frag
((typ console)(name fcm/loader_cli1.out))
```

If we try to use one of the inactive queries, it will fail. 

```frag
((typ console)(name fcm/loader_cli2.out))
```

But, we can load the `ls` handler with a config of our choice, at
which point, it will be available for use.  And once we unload it, it
will be unavailable yet again, and could be reloaded with a different
config.

```frag
((typ console)(name fcm/loader_cli3.out))
```

Notably, the loader can't be itself loaded (since it's not on the list
of known handlers), and can't be unloaded.

```frag
((typ console)(name fcm/loader_cli3.out))
```

We can push this dynamism yet further using libraries like
`ocaml_plugin`, which use OCaml's dynamic linking facilities to allow
a program to compile and load an OCaml source file as a first-class
module.  Thus, one could extend `Loader` to loads entirely new query
handlers from disk on demand.

## Living without first-class modules

It's worth noting that most designs that can be done with first-class
modules can be simulated without them, with some level of
awkwardness.  For example, we could rewrite our query handler example
without first-class modules using the following types:

```frag
((typ ocamltop)(name fcm/query_handler.topscript)(part 14))
```

The idea here is that we hide the true types of the objects in
question behind the functions stored in the closure.  Thus, we could
put the `Unique` query handler into this framework as follows.

```frag
((typ ocamltop)(name fcm/query_handler.topscript)(part 15))
```

For an example on this scale, the above approach is completely
reasonable, and first-class modules are not really necessary.  But the
more functionality you need to hide away behind a set of closures, and
the more complicated the relationships between the different types in
question, the more awkward this approach becomes, and the better it is
to use first-class modules.


