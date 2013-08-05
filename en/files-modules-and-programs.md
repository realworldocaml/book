Files, Modules and Programs
===========================

We've so far experienced OCaml largely through the toplevel.  As you
move from exercises to real-world programs, you'll need to leave the
toplevel behind and start building programs from files.  Files are
more than just a convenient way to store and manage your code; in
OCaml, they also act as boundaries that divide your program into
conceptual units.

In this chapter, we'll show you how to build an OCaml program from a
collection of files, as well as the basics of working with modules and
module signatures.

## Single File Programs ##

We'll start with an example: a utility that reads lines from `stdin`
and computes a frequency count of the lines that have been read in.
At the end, the 10 lines with the highest frequency counts are written
out.  We'll start with a simple implementation, which we'll save as
the file `freq.ml`.

This implementation will use two functions from the `List.Assoc`
module, which provides utility functions for interacting with
association lists, _i.e._, lists of key/value pairs.  In particular,
we use the function `List.Assoc.find`, which looks up a key in an
association list, and `List.add`, which adds a new binding to an
association list, as shown below.

```frag
((typ ocamltop)(name files-modules-and-programs/intro.topscript))
```

Note that `List.Assoc.add` doesn't modify the original list, but
instead allocates a new list with the requisite key/value added.

Now we can write down `freq.ml`.

```frag
((typ ocaml)(name files-modules-and-programs-freq/freq.ml))
```

The function `build_counts` reads in lines from `stdin`, constructing
from those lines an association list with the frequencies of each
line.  It does this by invoking `In_channel.fold_lines` (similar to
the function `List.fold` described in [xref](#lists-and-patterns)),
which reads through the lines one by one, calling the provided fold
function for each line to update the accumulator.  That accumulator is
initialized to the empty list.

With `build_counts` defined, we then call the function to build the
association list, sort that list by frequency in descending order,
grab the first 10 elements off the list, and then iterate over those
ten elements and print them to the screen.  These operations are tied
together using the `|>` operator, as described in
[xref](#variables-and-functions).


<note><title>Where is the main function?</title>

Unlike C, programs in OCaml do not have a unique `main` function.
When an OCaml program is evaluated, all the statements in the
implementation files are evaluated in the order in which they were
linked together.  These implementation files can contain arbitrary
expressions, not just function definitions. In this example, the
declaration starting with `let () =` plays the role of the `main`
declaration, kicking off the processing.  But really the entire file
is evaluated at startup, and so in some sense the full codebase is one
big `main` function.

</note>

If we weren't using Core or any other external libraries, we could
build the executable like this:

```frag
((typ console)(name files-modules-and-programs-freq/simple_build_fail.out))
```

But as you can see, it fails because it can't find Core.  We need a
somewhat more complex invocation to get Core linked in:

```frag
((typ console)(name files-modules-and-programs-freq/simple_build.out))
```

Here we're using `ocamlfind`, a tool which itself invokes other parts
of the OCaml toolchain (in this case, `ocamlc`) with the appropriate
flags to link in particular libraries and packages.  Here, `-package
core` is asking `ocamlfind` to link in the Core library, `-linkpkg`
asks ocamlfind to link in the packages as is necessary for building an
executable, while `-thread` turns on threading support, which is
required for Core.

While this works well enough for a one-file project, more complicated projects
require a tool to orchestrate the build.  One good tool for this task is
`ocamlbuild`, which is shipped with the OCaml compiler.  We'll talk more about
`ocamlbuild` in [xref](#the-compiler-frontend-parsing-and-type-checking), but
for now, we'll just use a simple wrapper around `ocamlbuild` called `corebuild`
that sets build parameters appropriately for building against Core and its
related libraries.

```frag
((typ console)(name files-modules-and-programs-freq/build.out))
```

If we'd invoked `ocamlbuild` with a target of `freq.native` instead of
`freq.byte`, we would have gotten native-code instead.

We can run the resulting executable from the command-line.  The
following line extracts strings from the `ocamlopt` binary, reporting
the most frequently occurring ones.  Note that the specific results
will vary from platform to platform, since the binary itself will
differ between platforms.

```frag
((typ console)(name files-modules-and-programs-freq/test.out))
```

<note><title>Bytecode vs native code</title>

OCaml ships with two compilers: the `ocamlc` bytecode compiler and the
`ocamlopt` native-code compiler.  Programs compiled with `ocamlc` are
interpreted by a virtual machine, while programs compiled with
`ocamlopt` are compiled to native machine code to be run on a specific
operating system and processor architecture.

Aside from performance, executables generated by the two compilers
have nearly identical behavior.  There are a few things to be aware
of.  First, the bytecode compiler can be used on more architectures,
and has some tools that are not available for native code.  For
example, the OCaml debugger only works with bytecode (although `gdb`,
the Gnu Debugger, works with OCaml native-code applications).  The
bytecode compiler is also quicker than the native-code compiler.  In
addition, in order to run a bytecode executable you typically need to
have OCaml installed on the system in question.  That's not strictly
required, though, since you can build a bytecode executable with an
embedded runtime, using the `-custom` compiler flag.

As a general matter, production executables should usually be built
using the native-code compiler, but it sometimes makes sense to use
bytecode for development builds.  And, of course, bytecode makes sense
when targeting a platform not supported by the native-code compiler.

</note>


## Multi-file programs and modules ##

Source files in OCaml are tied into the module system, with each file
compiling down into a module whose name is derived from the name of
the file.  We've encountered modules before, for example, when we used
functions like `find` and `add` from the `List.Assoc` module.  At its
simplest, you can think of a module as a collection of definitions
that are stored within a namespace.

Let's consider how we can use modules to refactor the implementation
of `freq.ml`.  Remember that the variable `counts` contains an
association list representing the counts of the lines seen so far.
But updating an association list takes time linear in the length of
the list, meaning that the time complexity of processing a file is
quadratic in the number of distinct lines in the file.

We can fix this problem by replacing association lists with a more
efficient data structure.  To do that, we'll first factor out the key
functionality into a separate module with an explicit interface.  We
can consider alternative (and more efficient) implementations once we
have a clear interface to program against.

We'll start by creating a file, `counter.ml` that contains the logic
for maintaining the association list used to describe the counts.  The
key function, called `touch`, updates the association list with the
information that a given line should be added to the frequency counts.


```frag
((typ ocaml)(name files-modules-and-programs-freq-with-counter/counter.ml))
```

The file `counter.ml` will be compiled into a module named `Counter`.
The name of the module is derived automatically from the filename.
The module name is capitalized even if the file is not, and more
generally module names must be capitalized.

We can now rewrite `freq.ml` to use `Counter`.  Note that the
resulting code can still be built with `ocamlbuild`, which will
discover dependencies and realize that `counter.ml` needs to be
compiled.

```frag
((typ ocaml)(name files-modules-and-programs-freq-with-counter/freq.ml))
```

## Signatures and Abstract Types

While we've pushed some of the logic to the `Counter` module, the code
in `freq.ml` can still depend on the details of the implementation of
`Counter`.  Indeed, if you look at the definition of `build_counts`,
you'll see that it depends on the fact that the empty set of frequency
counts is represented as an empty list.  We'd like to prevent this
kind of dependency so we can change the implementation of `Counter`
without needing to change client code like that in `freq.ml`.

The implementation details of a module can be hidden by attaching an
_interface_.  (Note that the terms _interface_, _signature_ and
_module type_ are all used interchangeably.)  A module defined by a
file `filename.ml` can be constrained by a signature placed in a file
called `filename.mli`.

For `counter.mli`, we'll start by writing down an interface that
describes what's currently available in `counter.ml`, without hiding
anything.  `val` declarations are used to specify values in a
signature.  The syntax of a `val` declaration is as follows:

```frag
((typ ocamlsyntax)(name files-modules-and-programs/val.syntax))
```

Using this syntax, we can write the signature of `counter.ml` as
follows.

```frag
((typ ocaml)(name files-modules-and-programs-freq-with-sig/counter.mli))
```

Note that `ocamlbuild` will detect the presence of the `mli` file
automatically and include it in the build.

<note><title>Auto-generating `mli` files</title>

If you don't want to construct an mli entirely by hand, you can ask
OCaml to auto-generate one for you from the source, which you can then
adjust to fit your needs.  Here's how you can do that using
`corebuild`.

```frag
((typ console)(name files-modules-and-programs-freq-with-counter/infer_mli.out))
```

The generated code is basically equivalent to the `mli` that we wrote
by hand, but is a bit uglier and more verbose, and, of course, has no
comments.  In general, auto-generated `mli`'s are only useful as a
starting point.  In OCaml, the `mli` is the key place where you
present and document your interface, and there's no replacement for
careful human editing and organization.

</note>


To hide the fact that frequency counts are represented as association
lists, we'll need to make the type of frequency counts _abstract_.  A
type is abstract if its name is exposed in the interface, but its
definition is not.  Here's an abstract interface for `Counter`:

```frag
((typ ocaml)(name files-modules-and-programs-freq-with-sig-abstract/counter.mli))
```

Note that we needed to add `empty` and `to_list` to `Counter`, since
otherwise, there would be no way to create a `Counter.t` or get data
out of one.  

We also used this opportunity to document the module.  The `mli` file
is the place where you specify your module's interface, and as such is
a natural place to document the module as well.  We also started our
comments with a double asterisk to cause them to be picked up by the
`ocamldoc` tool when generating API documentation.  We'll discuss
`ocamldoc` more in
[xref](#the-compiler-frontend-parsing-and-type-checking).

Here's a rewrite of `counter.ml` to match the new `counter.mli`.

```frag
((typ ocaml)(name files-modules-and-programs-freq-with-sig-abstract/counter.ml))
```

If we now try to compile `freq.ml`, we'll get the following error:

```frag
((typ console)(name files-modules-and-programs-freq-with-sig-abstract/build.out))
```

This is because `freq.ml` depends on the fact that frequency counts
are represented as association lists, a fact that we've just hidden.
We just need to fix `build_counts` to use `Counter.empty` instead of
`[]` and `Counter.to_list` to get the association list out at the end
for processing and printing.  The resulting implementation is shown
below.

```frag
((typ ocaml)(name files-modules-and-programs-freq-with-sig-abstract-fixed/freq.ml))
```

Now we can turn to optimizing the implementation of `Counter`.  Here's
an alternate and far more efficient implementation, based on the `Map`
datastructure in Core.

```frag
((typ ocaml)(name files-modules-and-programs-freq-fast/counter.ml))
```

Note that in the above we use `String.Map` in some places and simply
`Map` in others.  This has to do with the fact that for some
operations, like creating a `Map.t`, you need access to
type-specialized information, and for others, like looking something
up in `Map.t`, you don't.  This is covered in more detail in
[xref](#maps-and-hash-tables).

## Concrete types in signatures

In our frequency-count example, the module `Counter` had an abstract
type `Counter.t` for representing a collection of frequency counts.
Sometimes, you'll want to make a type in your interface _concrete_, by
including the type definition in the interface.

For example, imagine we wanted to add a function to `Counter` for
returning the line with the median frequency count.  If the number of
lines is even, then there is no precise median and the function would
return the lines before and after the median instead.  We'll use a
custom type to represent the fact that there are two possible return
values.  Here's a possible implementation.

```frag
((typ ocaml)
 (name files-modules-and-programs-freq-median/counter.ml)
 (part 1))
```

In the above, we use `failwith` to throw an exception for the case of
the empty list.  We'll discuss exceptions more in
[xref](#error-handling).  Note also that the function `fst` simply
returns the first element of any 2-tuple.

Now, to expose this usefully in the interface, we need to expose both
the function and the type `median` with its definition.  Note that
values (of which functions are an example) and types have distinct
namespaces, so there's no name clash here.  Adding the following two
lines added to `counter.mli` does the trick.

```frag
((typ ocaml)
 (name files-modules-and-programs-freq-median/counter.mli)
 (part 1))
```

The decision of whether a given type should be abstract or concrete is
an important one.  Abstract types give you more control over how
values are created and accessed, and make it easier to enforce
invariants beyond what is enforced by the type itself; concrete types
let you expose more detail and structure to client code in a
lightweight way.  The right choice depends very much on the context.

## Nested modules

Up until now, we've only considered modules that correspond to files,
like `counter.ml`.  But modules (and module signatures) can be nested
inside other modules.  As a simple example, consider a program that
needs to deal with multiple identifiers like usernames and hostnames.
If you just represent these as strings, then it becomes easy to
confuse one with the other.

A better approach is to mint new abstract types for each identifier,
where those types are under the covers just implemented as strings.
That way, the type system will prevent you from confusing a username
with a hostname, and if you do need to convert, you can do so using
explicit conversions to and from the string type.

Here's how you might create such an abstract type, within a
sub-module:

```frag
((typ ocaml)(name files-modules-and-programs/abstract_username.ml))
```

Note that the `to_string` and `of_string` functions above are
implemented simply as the identity function, which means they have no
runtime effect.  They are there purely as part of the discipline that
they enforce on the code through the type system.

The basic structure of a module declaration like this is:

```frag
((typ ocamlsyntax)(name files-modules-and-programs/module.syntax))
```

We could have written this slightly differently, by giving the
signature its own toplevel `module type` declaration, making it
possible to create multiple distinct types with the same underlying
implementation in a lightweight way.


```frag
((typ ocaml)(name files-modules-and-programs/session_info.ml))
```

The above code has a bug: it compares the username in one session to
the host in the other session, when it should be comparing the
usernames in both cases.  Because of how we defined our types,
however, the compiler will flag this bug for us.

```frag
((typ console)(name files-modules-and-programs/build_session_info.out))
```

## Opening modules

Most of the time, you refer to values and types within a module by
using the module name as an explicit qualifier.  _e.g._, you write
`List.map` to refer to the `map` function in the `List` module.
Sometimes, though, you want to be able to refer to the contents of a
module without this explicit qualification.  That's what the `open`
statement is for.

We've encountered `open` already, specifically where we've written
`open Core.Std` to get access to the standard definitions in the Core
library.  In general, opening a module adds the contents of that
module to the environment that the compiler looks at to find the
definition of various identifiers.  Here's an example.

```frag
((typ ocamltop)(name files-modules-and-programs/main.topscript)(part 0))
```

`open` is essential when you want to modify your environment for a
standard library like Core, but it's generally good style to keep
opening of modules to a minimum.  Opening a module is basically a
tradeoff between terseness and explicitness --- the more modules you
open, the fewer module qualifications you need, and the harder it is
to look at an identifier and figure out where it comes from.

Here's some general advice on how to deal with opens.

  * Opening modules at the toplevel of a module should be done quite
    sparingly, and generally only with modules that have been
    specifically designed to be opened, like `Core.Std` or
    `Option.Monad_infix`.

  * If you do need to do an open, it's better to do a _local open_.
    There are two syntaxes for local opens.  For example, you can
    write:

    ```frag
    ((typ ocamltop)(name files-modules-and-programs/main.topscript)(part 1))
    ```

    In the above, `of_int` and the infix operators are the ones from
    the `Int64` module.

    There's another even more lightweight syntax for local opens, which
    is particularly useful for small expressions:

    ```frag
    ((typ ocamltop)(name files-modules-and-programs/main.topscript)(part 2))
    ```

  * An alternative to local opens that makes your code terser without
    giving up on explicitness is to locally rebind the name of a
    module.  So, when using the `Counter.median` type, instead of
    writing:

    ```frag
    ((typ ocaml)(name files-modules-and-programs-freq-median/use_median_1.ml)(part 1))
    ```

    you could write:

    ```frag
    ((typ ocaml)(name files-modules-and-programs-freq-median/use_median_2.ml)(part 1))
    ```

    Because the module name `C` only exists for a short scope, it's
    easy to read and remember what `C` stands for.  Rebinding modules
    to very short names at the toplevel of your module is usually a
    mistake.

## Including modules

While opening a module affects the environment used to search for
identifiers, _including_ a module is a way of actually adding new
identifiers to a module proper.  Consider the following simple module
for representing a range of intervals.

```frag
((typ ocamltop)(name files-modules-and-programs/main.topscript)(part 3))
```

We can use the `include` directive to create a new, extended version
of the `Interval` module.

```frag
((typ ocamltop)(name files-modules-and-programs/main.topscript)(part 4))
```

The difference between `include` and `open` is that we've done more
than change how identifiers are searched for: we've changed what's in
the module.  If we'd used `open`, we'd have gotten a quite different
result.

```frag
((typ ocamltop)(name files-modules-and-programs/main.topscript)(part 5))
```

To consider a more realistic example, imagine you wanted to build an
extended version of the `List` module, where you've added some
functionality not present in the module as distributed in Core.
`include` allows us to do just that.

```frag
((typ ocaml)(name files-modules-and-programs/ext_list.ml))
```

Now, what about the interface of this new module?  It turns out that
include works on the signature language as well, so we can pull
essentially the same trick to write an `mli` for this new module.  The
only trick is that we need to get our hands on the signature for the
list module, which can be done using `module type of`.

```frag
((typ ocaml)(name files-modules-and-programs/ext_list.mli))
```

Note that the order of declarations in the `mli` does not need to
match the order of declarations in the `ml`.  Also, the order of
declarations in the `ml` is quite important in that it determines what
values are shadowed.  If we wanted to replace a function in `List`
with a new function of the same name, the declaration of that function
in the `ml` would have to come after the `include List` declaration.

And we can now use `Ext_list` as a replacement for `List`.  If we want
to use `Ext_list` in preference to `List` in our project, we can
create a file of common definitions:

```frag
((typ ocaml)(name files-modules-and-programs/common.ml))
```

And if we then put `open Common` after `open Core.Std` at the top of
each file in our project, then references to `List` will automatically
go to `Ext_list` instead.

## Common errors with modules

When OCaml compiles a program with an `ml` and an `mli`, it will
complain if it detects a mismatch between the two.  Here are some of
the common errors you'll run into.

### Type mismatches

The simplest kind of error is where the type specified in the
signature does not match up with the type in the implementation of the
module.  As an example, if we replace the `val` declaration in
`counter.mli` by swapping the types of the first two arguments:

```frag
((typ ocaml)(name files-modules-and-programs-freq-with-sig-mismatch/counter.mli)(part 1))
```

and we try to compile, we'll get the following error.

```frag
((typ console)(name files-modules-and-programs-freq-with-sig-mismatch/build.out))
```

This error message is a bit intimidating at first, and it takes a bit
of thought to see why the first type for touch (which comes from the
implementation) doesn't match the second one (which comes from the
interface).  The key thing to remember is that `t` is a
`Core.Std.Map.t`, at which point you can see that the error is a
mismatch in the order of arguments to `touch`.

There's no denying that learning to decode such error messages is
difficult at first, and takes some getting used to.  But in time,
decoding these errors becomes second nature.

### Missing definitions

We might decide that we want a new function in `Counter` for pulling
out the frequency count of a given string.  We can update the `mli` by
adding the following line.

```frag
((typ ocaml)
 (name files-modules-and-programs-freq-with-missing-def/counter.mli)
 (part 1))
```

Now, if we try to compile without actually adding the implementation,
we'll get this error:

```frag
((typ console)
 (name files-modules-and-programs-freq-with-missing-def/build.out)
) 
```

A missing type definition will lead to a similar error.

### Type definition mismatches

Type definitions that show up in an `mli` need to match up with
corresponding definitions in the `ml`.  Consider again the example of
the type `median`.  The order of the declaration of variants matters
to the OCaml compiler, so the definition of `median` in the
implementation listing those options in a different order:

```frag
((typ ocaml)
 (name files-modules-and-programs-freq-with-type-mismatch/counter.mli)
 (part 1))
```

will lead to a compilation error:

```frag
((typ console)
 (name files-modules-and-programs-freq-with-type-mismatch/build.out)
) 
```

Order is similarly important in other parts of the signature,
including the order in which record fields are declared and the order
of arguments (including labeled and optional arguments) to a
function.

### Cyclic dependencies

In most cases, OCaml doesn't allow cyclic dependencies, _i.e._, a
collection of definitions that all refer to each other.  If you want
to create such definitions, you typically have to mark them specially.
For example, when defining a set of mutually recursive values (like
the definition of `is_even` and `is_odd` in
[xref](#recursive-functions)), you need to define them using `let rec`
rather than ordinary `let`.

The same is true at the module level.  By default, cyclic dependencies
between modules are not allowed, and indeed, cyclic dependencies among
files are never allowed.  Recursive modules are possible, but are a
rare case and we won't discuss them further here.

The simplest case of this is that a module can not directly refer to
itself (although definitions within a module can refer to each other
in the ordinary way).  So, if we tried to add a reference to `Counter`
from within `counter.ml`

```frag
((typ ocaml)
 (name files-modules-and-programs-freq-cyclic1/counter.ml)
 (part 1))
```

we'll see this error when we try to build:

```frag
((typ console)
 (name files-modules-and-programs-freq-cyclic1/build.out))
```

The problem manifests in a different way if we create cyclic
references between files.  We could create such a situation by adding
a reference to `Freq` from `counter.ml`, _e.g._, by adding the
following line.

```frag
((typ ocaml)
 (name files-modules-and-programs-freq-cyclic2/counter.ml)
 (part 1))
```

In this case, `ocamlbuild` (which is invoked by the `corebuild`
script) will notice the error and complain explicitly about the cycle.

```frag
((typ console)
 (name files-modules-and-programs-freq-cyclic2/build.out))
```

