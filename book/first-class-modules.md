# First-Class Modules {#first-class-modules data-type=chapter}

You can think of OCaml as being broken up into two parts: a core language
that is concerned with values and types, and a module language that is
concerned with modules and module signatures. These sublanguages are
stratified, in that modules can contain types and values, but ordinary values
can't contain modules or module types. That means you can't do things like
define a variable whose value is a module, or a function that takes a module
as an argument. [modules/first-class modules]{.idx #MODfirst}

OCaml provides a way around this stratification in the form of
*first-class modules*. First-class modules are ordinary values that can be
created from and converted back to regular modules. [first-class
modules/working with]{.idx #FCMwork}

First-class modules are a sophisticated technique, and you'll need to get
comfortable with some advanced aspects of the language to use them
effectively. But it's worth learning, because letting modules into the core
language is quite powerful, increasing the range of what you can express and
making it easier to build flexible and modular
<span class="keep-together">systems</span>.

## Working with First-Class Modules {#working-with-first-class-modules data-type=sect1}

We'll start out by covering the basic mechanics of first-class modules by
working through some toy examples. We'll get to more realistic examples in
the next section.

In that light, consider the following signature of a module with a single
integer variable:

<link rel="import" href="code/fcm/main.mlt" part="0.5" />

We can also create a module that matches this signature:

<link rel="import" href="code/fcm/main.mlt" part="1" />

A first-class module is created by packaging up a module with a signature
that it satisfies. This is done using the `module` keyword. [module
keyword]{.idx}

<link rel="import" href="code/fcm/pack.syntax" />

We can convert `Three` into a first-class module as follows:

<link rel="import" href="code/fcm/main.mlt" part="2" />

The module type doesn't need to be part of the construction of a first-class
module if it can be inferred. Thus, we can write:

<link rel="import" href="code/fcm/main.mlt" part="3" />

We can also create a first-class module from an anonymous module:

<link rel="import" href="code/fcm/main.mlt" part="4" />

In order to access the contents of a first-class module, you need to unpack
it into an ordinary module. This can be done using the `val` keyword, using
this syntax:

<link rel="import" href="code/fcm/unpack.syntax" />

Here's an example:

<link rel="import" href="code/fcm/main.mlt" part="5" />

We can also write ordinary functions which consume and create first-class
modules. The following shows the definition of two functions: `to_int`, which
converts a `(module X_int)` into an `int`; and `plus`, which returns the sum
of two `(module X_int)`:

<link rel="import" href="code/fcm/main.mlt" part="8" />

With these functions in hand, we can now work with values of type
`(module X_int)` in a more natural style, taking advantage of the concision
and simplicity of the core <span class="keep-together">language</span>:

<link rel="import" href="code/fcm/main.mlt" part="9" />

There are some useful syntactic shortcuts when dealing with first-class
modules. One notable one is that you can do the conversion to an ordinary
module within a pattern match. Thus, we can rewrite the `to_int` function as
follows:

<link rel="import" href="code/fcm/main.mlt" part="10" />

First-class modules can contain types and functions in addition to simple
values like `int`. Here's an interface that contains a type and a
corresponding `bump` operation that takes a value of the type and produces a
new one:

<link rel="import" href="code/fcm/main.mlt" part="11" />

We can create multiple instances of this module with different underlying
types:

<link rel="import" href="code/fcm/main.mlt" part="12" />

And we can convert these to first-class modules:

<link rel="import" href="code/fcm/main.mlt" part="13" />

But you can't do much with `int_bumper`, since `int_bumper` is fully
abstract, so that we can no longer recover the fact that the type in question
is `int`.

<link rel="import" href="code/fcm/main.mlt" part="14" />

To make `int_bumper` usable, we need to expose the type, which we can do as
follows:

<link rel="import" href="code/fcm/main.mlt" part="15" />

The sharing constraints we've added above make the resulting first-class
modules <span class="keep-together">polymorphic</span> in the type `t`. As a
result, we can now use these first-class modules on values of the matching
type:

<link rel="import" href="code/fcm/main.mlt" part="16" />

We can also write functions that use such first-class modules
polymorphically. The following function takes two arguments: a `Bumpable`
module and a list of elements of the same type as the type `t` of the module:
[polymorphism/in first-class modules]{.idx}[first-class modules/polymorphism
in]{.idx}

<link rel="import" href="code/fcm/main.mlt" part="17" />

Here, we used a feature of OCaml that hasn't come up before: a
*locally abstract type*. For any function, you can declare a pseudoparameter
of the form `(type a)` which introduces a fresh type named `a`. This type
acts like an abstract type within the context of the function. In the example
above, the locally abstract type was used as part of a sharing constraint
that ties the type `B.t` with the type of the elements of the list passed in.
[datatypes/locally abstract types]{.idx}[abstract types]{.idx}[locally
abstract types]{.idx}[sharing constraint]{.idx}

The resulting function is polymorphic in both the type of the list element
and the type `Bumpable.t`. We can see this function in action:

<link rel="import" href="code/fcm/main.mlt" part="18" />

Polymorphic first-class modules are important because they allow you to
connect the types associated with a first-class module to the types of other
values you're working with.

::: {data-type=note}
### More on Locally Abstract Types

One of the key properties of locally abstract types is that they're dealt
with as abstract types in the function they're defined within, but are
polymorphic from the outside. Consider the following example:
[polymorphism/in locally abstract types]{.idx}

<link rel="import" href="code/fcm/main.mlt" part="19" />

This compiles successfully because the type `a` is used in a way that is
compatible with it being abstract, but the type of the function that is
inferred is polymorphic.

If, on the other hand, we try to use the type `a` as equivalent to some
concrete type, say, `int`, then the compiler will complain:

<link rel="import" href="code/fcm/main.mlt" part="20" />

One common use of locally abstract types is to create a new type that can be
used in constructing a module. Here's an example of doing this to create a
new first-class module:

<link rel="import" href="code/fcm/main.mlt" part="21" />

Here, what we effectively do is capture a polymorphic type and export it as a
concrete type within a module.

This technique is useful beyond first-class modules. For example, we can use
the same approach to construct a local module to be fed to a functor.
<a data-type="indexterm" data-startref="FCMwork">&nbsp;</a>
:::


## Example: A Query-Handling Framework {#example-a-query-handling-framework data-type=sect1}

Now let's look at first-class modules in the context of a more complete and
realistic example. In particular, consider the following signature for a
module that implements a system for responding to user-generated queries.
[query-handlers/and first-class modules]{.idx}[first-class
modules/query-handling framework]{.idx #FCMquery}

<link rel="import" href="code/fcm/query_handler.mlt" part="0.5" />

Here, we used s-expressions as the format for queries and responses, as well
as the configuration for the query handler. S-expressions are a simple,
flexible, and human-readable serialization format commonly used in Core. For
now, it's enough to think of them as balanced parenthetical expressions whose
atomic values are strings, e.g.,
`(this (is an) (s expression))`.[s-expressions/in queries and
responses]{.idx}

In addition, we use the `ppx_sexp_conv` syntax extension which interprets the
`[@@deriving_sexp]` annotation. When `ppx_sexp_conv` sees `[@@deriving sexp]`
attached to a signature, it replaces it with declarations of s-expression
converters, for example:[sexp declaration]{.idx}

<link rel="import" href="code/fcm/query_handler.mlt" part="1" />

In a module, `[@@deriving sexp]` adds the implementation of those functions.
Thus, we can write:

<link rel="import" href="code/fcm/query_handler.mlt" part="2" />

This is all described in more detail in
[Data Serialization With S Expressions](data-serialization.html#data-serialization-with-s-expressions){data-type=xref}.

### Implementing a Query Handler {#implementing-a-query-handler}

Let's look at some examples of query handlers that satisfy the
`Query_handler` interface. The first example is a handler that produces
unique integer IDs. It works by keeping an internal counter which it bumps
every time it produces a new value. The input to the query in this case is
just the trivial s-expression `()`, otherwise known as `Sexp.unit`:
[query-handlers/implementation of]{.idx}

<link rel="import" href="code/fcm/query_handler.mlt" part="3" />

We can use this module to create an instance of the `Unique` query handler
and interact with it directly:

<link rel="import" href="code/fcm/query_handler.mlt" part="4" />

Here's another example: a query handler that does directory listings. Here,
the config is the default directory that relative paths are interpreted
within:

<link rel="import" href="code/fcm/query_handler.mlt" part="5" />

Again, we can create an instance of this query handler and interact with it
directly:

<link rel="import" href="code/fcm/query_handler.mlt" part="6" />

### Dispatching to Multiple Query Handlers {#dispatching-to-multiple-query-handlers}

Now, what if we want to dispatch queries to any of an arbitrary collection of
handlers? Ideally, we'd just like to pass in the handlers as a simple data
structure like a list. This is awkward to do with modules and functors alone,
but it's quite natural with first-class modules. The first thing we'll need
to do is create a signature that combines a `Query_handler` module with an
instantiated query handler:[query-handlers/dispatching to multiple]{.idx}

<link rel="import" href="code/fcm/query_handler.mlt" part="7" />

With this signature, we can create a first-class module that encompasses both
an instance of the query and the matching operations for working with that
query.

We can create an instance as follows:

<link rel="import" href="code/fcm/query_handler.mlt" part="8" />

Constructing instances in this way is a little verbose, but we can write a
function that eliminates most of this boilerplate. Note that we are again
making use of a locally abstract type:

<link rel="import" href="code/fcm/query_handler.mlt" part="9" />

Using `build_instance`, constructing a new instance becomes a one-liner:

<link rel="import" href="code/fcm/query_handler.mlt" part="10" />

We can now write code that lets you dispatch queries to one of a list of
query handler instances. We assume that the shape of the query is as follows:

<link rel="import" href="code/fcm/query-syntax.scm" />

where *`query-name`* is the name used to determine which query handler to
dispatch the query to, and *`query`* is the body of the query.

The first thing we'll need is a function that takes a list of query handler
instances and constructs a dispatch table from it:

<link rel="import" href="code/fcm/query_handler.mlt" part="11" />

Now, we need a function that dispatches to a handler using a dispatch table:

<link rel="import" href="code/fcm/query_handler.mlt" part="12" />

This function interacts with an instance by unpacking it into a module 
`I` and then using the query handler instance (`I.this`) in concert with the
associated module (`I.Query_handler`).[I.Query_handler module]{.idx}

The bundling together of the module and the value is in many ways reminiscent
of object-oriented languages. One key difference, is that first-class modules
allow you to package up more than just functions or methods. As we've seen,
you can also include types and even modules. We've only used it in a small
way here, but this extra power allows you to build more sophisticated
components that involve multiple interdependent types and values.

Now let's turn this into a complete, running example by adding a command-line
interface:

<link rel="import" href="code/fcm/query_handler.mlt" part="13" />

We can most effectively run this command-line interface from a standalone
program, which we can do by putting the above code in a file along with
following command to launch the interface:

<link rel="import" href="code/fcm/query_handler_loader/query_handler.ml" part=
"1" />

Here's an example of a session with this program:

<link rel="import" href="code/fcm/query_example.rawscript" />

### Loading and Unloading Query Handlers {#loading-and-unloading-query-handlers}

One of the advantages of first-class modules is that they afford a great deal
of dynamism and flexibility. For example, it's a fairly simple matter to
change our design to allow query handlers to be loaded and unloaded at
runtime.[query-handlers/loading/unloading of]{.idx}

We'll do this by creating a query handler whose job is to control the set of
active query handlers. The module in question will be called `Loader`, and
its configuration is a list of known `Query_handler` modules. Here are the
basic types:

<link rel="import" href="code/fcm/query_handler_loader/query_handler_core.ml" part=
"1" />

Note that a `Loader.t` has two tables: one containing the known query handler
modules, and one containing the active query handler instances. The
`Loader.t` will be responsible for creating new instances and adding them to
the table, as well as for removing instances, all in response to user
queries.

Next, we'll need a function for creating a `Loader.t`. This function requires
the list of known query handler modules. Note that the table of active
modules starts out as empty:

<link rel="import" href="code/fcm/query_handler_loader/query_handler_core.ml" part=
"2" />

Now we'll start writing out the functions for manipulating the table of
active query handlers. We'll start with the function for loading an instance.
Note that it takes as an argument both the name of the query handler and the
configuration for instantiating that handler in the form of an s-expression.
These are used for creating a first-class module of type
`(module Query_handler_instance)`, which is then added to the active table:

<link rel="import" href="code/fcm/query_handler_loader/query_handler_core.ml" part=
"3" />

Since the `load` function will refuse to `load` an already active handler, we
also need the ability to unload a handler. Note that the handler explicitly
refuses to unload itself:

<link rel="import" href="code/fcm/query_handler_loader/query_handler_core.ml" part=
"4" />

Finally, we need to implement the `eval` function, which will determine the
query <span class="keep-together">interface</span> presented to the user.
We'll do this by creating a variant type, and using the s-expression
converter generated for that type to parse the query from the user:

<link rel="import" href="code/fcm/query_handler_loader/query_handler_core.ml" part=
"5" />

The `eval` function itself is fairly straightforward, dispatching to the
appropriate functions to respond to each type of query. Note that we write
`<:sexp_of<string list>>` to autogenerate a function for converting a list of
strings to an s-expression, as described in
[Data Serialization With S Expressions](data-serialization.html#data-serialization-with-s-expressions){data-type=xref}.

This function ends the definition of the `Loader` module:

<link rel="import" href="code/fcm/query_handler_loader/query_handler_core.ml" part=
"6" />

Finally, we can put this all together with the command-line interface. We
first create an instance of the loader query handler and then add that
instance to the loader's active table. We can then just launch the
command-line interface, passing it the active table:

<link rel="import" href="code/fcm/query_handler_loader/query_handler_loader.ml" part=
"1" />

Now build this into a command-line interface to experiment with it:

<link rel="import" href="code/fcm/query_handler_loader/jbuild" />

<link rel="import" href="code/fcm/query_handler_loader/build_query_handler_loader.sh" />

The resulting command-line interface behaves much as you'd expect, starting
out with no query handlers available but giving you the ability to load and
unload them. Here's an example of it in action. As you can see, we start out
with `loader` itself as the only active handler:

<link rel="import" href="code/fcm/loader_cli1.rawsh" />

Any attempt to use an inactive query handler will fail:

<link rel="import" href="code/fcm/loader_cli2.rawsh" />

But, we can load the `ls` handler with a config of our choice, at which point
it will be available for use. And once we unload it, it will be unavailable
yet again and could be reloaded with a different config:

<link rel="import" href="code/fcm/loader_cli3.rawsh" />

Notably, the loader can't be loaded (since it's not on the list of known
handlers) and can't be unloaded either:

<link rel="import" href="code/fcm/loader_cli4.rawsh" />

Although we won't describe the details here, we can push this dynamism yet
further using OCaml's dynamic linking facilities, which allow you to compile
and link in new code to a running program. This can be automated using
libraries like `ocaml_plugin`, which can be installed via OPAM, and which
takes care of much of the workflow around setting up dynamic linking.
<a data-type="indexterm" data-startref="FCMquery">&nbsp;</a>


## Living Without First-Class Modules {#living-without-first-class-modules data-type=sect1}

It's worth noting that most designs that can be done with first-class modules
can be simulated without them, with some level of awkwardness. For example,
we could rewrite our query handler example without first-class modules using
the following types:[first-class modules/alternatives to]{.idx}

<link rel="import" href="code/fcm/query_handler.mlt" part="14" />

The idea here is that we hide the true types of the objects in question
behind the functions stored in the closure. Thus, we could put the `Unique`
query handler into this framework as follows:

<link rel="import" href="code/fcm/query_handler.mlt" part="15" />

For an example on this scale, the preceding approach is completely
reasonable, and first-class modules are not really necessary. But the more
functionality you need to hide away behind a set of closures, and the more
complicated the relationships between the different types in question, the
more awkward this approach becomes, and the better it is to use first-class
modules. <a data-type="indexterm" data-startref="MODfirst">&nbsp;</a>


