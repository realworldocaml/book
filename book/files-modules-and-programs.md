# Files, Modules, and Programs {#files-modules-and-programs data-type=chapter}

We've so far experienced OCaml largely through the toplevel. As you move from
exercises to real-world programs, you'll need to leave the toplevel behind
and start building programs from files. Files are more than just a convenient
way to store and manage your code; in OCaml, they also correspond to modules,
which act as boundaries that divide your program into conceptual units.

In this chapter, we'll show you how to build an OCaml program from a
collection of files, as well as the basics of working with modules and module
signatures.

## Single-File Programs {#single-file-programs data-type=sect1}

We'll start with an example: a utility that reads lines from `stdin`,
computes a frequency count of the lines, and prints out the ten most frequent
lines. We'll start with a simple implementation, which we'll save as the file
*freq.ml*. [files/single-file
programs]{.idx #FILEsnglprog}[programs/single-file programs]{.idx #Psingfil}

This implementation will use two functions from the `List.Assoc` module,
which provides utility functions for interacting with *association lists*,
*i.e.*, lists of key/value pairs. In particular, we use the function
`List.Assoc.find`, which looks up a key in an association list; and
`List.Assoc.add`, which adds a new binding to an association list, as shown
here: [List.Assoc module/List.Assoc.add]{.idx}[List.Assoc
module/List.Assoc.find]{.idx}[lists/adding new bindings
in]{.idx}[lists/finding key associations in]{.idx}

<link rel="import" href="code/files-modules-and-programs/intro.mlt" part=
"1" />

Note that `List.Assoc.add` doesn't modify the original list, but instead
allocates a new list with the requisite key/value pair added.

Now we can write `freq.ml`.

<link rel="import" href="code/files-modules-and-programs/freq/freq.ml" />

The function `build_counts` reads in lines from `stdin`, constructing from
those lines an association list with the frequencies of each line. It does
this by invoking `In_channel.fold_lines` (similar to the function `List.fold`
described in
[Lists And Patterns](lists-and-patterns.html#lists-and-patterns){data-type=xref}),
which reads through the lines one by one, calling the provided `fold`
function for each line to update the accumulator. That accumulator is
initialized to the empty list.

With `build_counts` defined, we then call the function to build the
association list, sort that list by frequency in descending order, grab the
first 10 elements off the list, and then iterate over those 10 elements and
print them to the screen. These operations are tied together using the 
`|>` operator described in
[Variables And Functions](variables-and-functions.html#prefix-and-infix-operators){data-type=xref}.
[let ( ) declaration]{.idx}[main function]{.idx}

::: {.allow_break data-type=note}
### Where Is `main`?

Unlike programs in C, Java or C#, programs in OCaml don't have a unique
`main` function. When an OCaml program is evaluated, all the statements in
the implementation files are evaluated in the order in which they were linked
together. These implementation files can contain arbitrary expressions, not
just function definitions. In this example, the declaration starting with
`let () =` plays the role of the `main` function, kicking off the processing.
But really the entire file is evaluated at startup, and so in some sense the
full codebase is one big `main` function.

The idiom of writing `let () =` may seem a bit odd, but it has a purpose. The
`let` binding here is a pattern-match to a value of type `unit`, which is
there to ensure that the expression on the righthand side returns `unit`, as
is common for functions that operate primarily by side effect.
:::


If we weren't using `Base` or any other external libraries, we could build
the executable like this:

<link rel="import" href="code/files-modules-and-programs/freq/simple_build_fail.errsh" />

But as you can see, it fails because it can't find `Base` and `Stdio`. We
need a somewhat more complex invocation to get them linked in: [OCaml
toolchain/ocamlc]{.idx}[OCaml toolchain/ocamlfind]{.idx}[Base standard
library/finding with ocamlfind]{.idx}

<link rel="import" href="code/files-modules-and-programs/freq/simple_build.sh" />

This uses `ocamlfind`, a tool which itself invokes other parts of the OCaml
toolchain (in this case, `ocamlc`) with the appropriate flags to link in
particular libraries and packages. Here, `-package base` is asking
`ocamlfind` to link in the `Base` library; `-linkpkg` asks ocamlfind to link
in the packages as is necessary for building an executable, while `-thread`
turns on threading support, which is required for Base. [threads/turning on
with -thread]{.idx}[-linkpkg]{.idx data-primary-sortas=linkpkg}

While this works well enough for a one-file project, more complicated
projects require a tool to orchestrate the build. One good tool for this task
is `jbuilder`. To invoke `jbuilder`, you need to have a `jbuild` file that
specifies the details of the build. [jbuilder]{.idx}

<link rel="import" href="code/files-modules-and-programs/freq-obuild/jbuild" />

With that in place, we can invoke `jbuilder` as follows.

<link rel="import" href="code/files-modules-and-programs/freq-obuild/freq.sh" part=
"build" />

We can run the resulting executable, `freq.bc`, from the command line. The
following invocation extracts strings from the `ocamlopt` binary, reporting
the most frequently occurring ones. Note that the specific results will vary
from platform to platform, since the binary itself will differ between
platforms. [OCaml toolchain/jbuilder]{.idx}[native-code compiler/vs. bytecode
compiler]{.idx}[bytecode compiler/vs. native-code compiler]{.idx}[OCaml
toolchain/ocamlopt]{.idx}[OCaml toolchain/ocamlc]{.idx}[code
compilers/bytecode vs. native
code]{.idx}<a data-type="indexterm" data-startref="FILEsnglprog">&nbsp;</a><a data-type="indexterm" data-startref="Psingfil">&nbsp;</a>

<link rel="import" href="code/files-modules-and-programs/freq-obuild/freq.sh" part=
"test" />

::: {data-type=note}
### Bytecode Versus Native Code

OCaml ships with two compilers: the `ocamlc` bytecode compiler and the
`ocamlopt` native-code compiler. Programs compiled with `ocamlc` are
interpreted by a virtual machine, while programs compiled with `ocamlopt` are
compiled to native machine code to be run on a specific operating system and
processor architecture. With `jbuilder`, targets ending with `.bc` are built
as bytecode executables, and those ending with `.exe` are built as native
code.

Aside from performance, executables generated by the two compilers have
nearly identical behavior. There are a few things to be aware of. First, the
bytecode compiler can be used on more architectures, and has some tools that
are not available for native code. For example, the OCaml debugger only works
with bytecode (although `gdb`, the GNU Debugger, works with OCaml native-code
applications). The bytecode compiler is also quicker than the native-code
compiler. In addition, in order to run a bytecode executable, you typically
need to have OCaml installed on the system in question. That's not strictly
required, though, since you can build a bytecode executable with an embedded
runtime, using the `-custom` compiler flag.

As a general matter, production executables should usually be built using the
native-code compiler, but it sometimes makes sense to use bytecode for
development builds. And, of course, bytecode makes sense when targeting a
platform not supported by the native-code compiler. We'll cover both
compilers in more detail in
[The Compiler Backend: Byte Code And Native Code](compiler-backend.html#the-compiler-backend-byte-code-and-native-code){data-type=xref}.
:::


## Multifile Programs and Modules {#multi-file-programs-and-modules data-type=sect1}

Source files in OCaml are tied into the module system, with each file
compiling down into a module whose name is derived from the name of the file.
We've encountered modules before, such as when we used functions like 
`find` and `add` from the `List.Assoc` module. At its simplest, you can think
of a module as a collection of definitions that are stored within a
namespace. [modules/basics of]{.idx}[files/multi-file
programs]{.idx}[programs/multi-file programs]{.idx}

Let's consider how we can use modules to refactor the implementation of
`freq.ml`. Remember that the variable `counts` contains an association list
representing the counts of the lines seen so far. But updating an association
list takes time linear in the length of the list, meaning that the time
complexity of processing a file is quadratic in the number of distinct lines
in the file.

We can fix this problem by replacing association lists with a more efficient
data structure. To do that, we'll first factor out the key functionality into
a separate module with an explicit interface. We can consider alternative
(and more efficient) implementations once we have a clear interface to
program against.

We'll start by creating a file, `counter.ml`, that contains the logic for
maintaining the association list used to represent the frequency counts. The
key function, called `touch`, bumps the frequency count of a given line by
one.

<link rel="import" href="code/files-modules-and-programs/freq-with-counter/counter.ml" />

The file *counter.ml* will be compiled into a module named `Counter`, where
the name of the module is derived automatically from the filename. The module
name is capitalized even if the file is not. Indeed, module names are always
capitalized. [modules/naming of]{.idx}

We can now rewrite `freq.ml` to use `Counter`.

<link rel="import" href="code/files-modules-and-programs/freq-with-counter/freq.ml" />

The resulting code can still be built with `jbuilder`, which will discover
dependencies and realize that `counter.ml` needs to be compiled.

<link rel="import" href="code/files-modules-and-programs/freq-with-counter/build.sh" />

## Signatures and Abstract Types {#signatures-and-abstract-types data-type=sect1}

While we've pushed some of the logic to the `Counter` module, the code in
`freq.ml` can still depend on the details of the implementation of `Counter`.
Indeed, if you look at the definition of `build_counts`, you'll see that it
depends on the fact that the empty set of frequency counts is represented as
an empty list. We'd like to prevent this kind of dependency, so we can change
the implementation of `Counter` without needing to change client code like
that in `freq.ml`. [abstract types]{.idx}[modules/module
type]{.idx}[signatures/abstract types]{.idx}[interfaces/hiding implementation
details with]{.idx}[modules/hiding implementation details]{.idx}

The implementation details of a module can be hidden by attaching an
*interface*. (Note that in the context of OCaml, the terms *interface*,
*signature*, and *module type* are all used interchangeably.) A module
defined by a file `filename.ml` can be constrained by a signature placed in a
file called `filename.mli`. [interfaces/synonyms for]{.idx}

For `counter.mli`, we'll start by writing down an interface that describes
what's currently available in `counter.ml`, without hiding anything. 
`val` declarations are used to specify values in a signature. The syntax of a
`val` declaration is as follows:

<link rel="import" href="code/files-modules-and-programs/val.syntax" />

Using this syntax, we can write the signature of `counter.ml` as follows.

<link rel="import" href="code/files-modules-and-programs/freq-with-sig/counter.mli" />

Note that `jbuilder` will detect the presence of the `mli` file automatically
and include it in the build.

To hide the fact that frequency counts are represented as association lists,
we'll need to make the type of frequency counts *abstract*. A type is
abstract if its name is exposed in the interface, but its definition is not.
Here's an abstract interface for `Counter`:

<link rel="import" href="code/files-modules-and-programs/freq-with-sig-abstract/counter.mli" />

Note that we needed to add `empty` and `to_list` to `Counter`, since
otherwise there would be no way to create a `Counter.t` or get data out of
one.

We also used this opportunity to document the module. The `mli` file is the
place where you specify your module's interface, and as such is a natural
place to put documentation. We started our comments with a double asterisk to
cause them to be picked up by the `odoc` tool when generating API
documentation. We'll discuss `odoc` more in
[The Compiler Frontend Parsing And Type Checking](compiler-frontend.html#the-compiler-frontend-parsing-and-type-checking){data-type=xref}.

Here's a rewrite of `counter.ml` to match the new `counter.mli`:

<link rel="import" href="code/files-modules-and-programs/freq-with-sig-abstract/counter.ml" />

If we now try to compile `freq.ml`, we'll get the following error:

<link rel="import" href="code/files-modules-and-programs/freq-with-sig-abstract/build.errsh" />

This is because `freq.ml` depends on the fact that frequency counts are
represented as association lists, a fact that we've just hidden. We just need
to fix `build_counts` to use `Counter.empty` instead of `[]` and to use
`Counter.to_list` to convert the completed counts to an association list. The
resulting implementation is shown below.

<link rel="import" href="code/files-modules-and-programs/freq-with-sig-abstract-fixed/freq.ml" />

With this implementation, the build now succeeds!

<link rel="import" href="code/files-modules-and-programs/freq-with-sig-abstract-fixed/build.sh" />

Now we can turn to optimizing the implementation of `Counter`. Here's an
alternate and far more efficient implementation, based on the `Map` data
structure in `Core_kernel`.

<link rel="import" href="code/files-modules-and-programs/freq-fast/counter.ml" />

There's some unfamiliar syntax in the above example, in particular, writing
`Map.M(String).t` to refer to the type of a map with string keys, and
`Map.empty (module String)` to generate an empty map. Here, we're making use
of some more advanced features of the language (specifically, *functors* and
*first-class modules*, which we'll get to in later chapters). The use of
these features for the Map data-structure in particular is covered in
[Maps And Hash Tables](maps-and-hashtables.html#maps-and-hash-tables){data-type=xref}.

## Concrete Types in Signatures {#concrete-types-in-signatures data-type=sect1}

In our frequency-count example, the module `Counter` had an abstract type
`Counter.t` for representing a collection of frequency counts. Sometimes,
you'll want to make a type in your interface *concrete*, by including the
type definition in the interface. [concrete types]{.idx}[signatures/concrete
types]{.idx}

For example, imagine we wanted to add a function to `Counter` for returning
the line with the median frequency count. If the number of lines is even,
then there is no precise median, and the function would return the lines
before and after the median instead. We'll use a custom type to represent the
fact that there are two possible return values. Here's a possible
implementation:

<link rel="import" href="code/files-modules-and-programs/freq-median/counter.ml" part=
"1" />

In the above, we use `failwith` to throw an exception for the case of the
empty list. We'll discuss exceptions more in
[Error Handling](error-handling.html#error-handling){data-type=xref}. Note
also that the function `fst` simply returns the first element of any
two-tuple.

Now, to expose this usefully in the interface, we need to expose both the
function and the type `median` with its definition. Note that values (of
which functions are an example) and types have distinct namespaces, so
there's no name clash here. Adding the following two lines to `counter.mli`
does the trick.

<link rel="import" href="code/files-modules-and-programs/freq-median/counter.mli" part=
"1" />

The decision of whether a given type should be abstract or concrete is an
important one. Abstract types give you more control over how values are
created and accessed, and make it easier to enforce invariants beyond what is
enforced by the type itself; concrete types let you expose more detail and
structure to client code in a lightweight way. The right choice depends very
much on the context.

## Nested Modules {#nested-modules data-type=sect1}

Up until now, we've only considered modules that correspond to files, like
`counter.ml`. But modules (and module signatures) can be nested inside other
modules. As a simple example, consider a program that needs to deal with
multiple identifiers like usernames and hostnames. If you just represent
these as strings, then it becomes easy to confuse one with the other.
[identifiers/dealing with multiple]{.idx}[nested
modules]{.idx}[modules/nested modules]{.idx}

A better approach is to mint new abstract types for each identifier, where
those types are under the covers just implemented as strings. That way, the
type system will prevent you from confusing a username with a hostname, and
if you do need to convert, you can do so using explicit conversions to and
from the string type.

Here's how you might create such an abstract type, within a submodule:
[abstract types]{.idx}

<link rel="import" href="code/files-modules-and-programs/abstract_username.ml" />

Note that the `to_string` and `of_string` functions above are implemented
simply as the identity function, which means they have no runtime effect.
They are there purely as part of the discipline that they enforce on the code
through the type system. We also chose to put in an equality function, so you
can check if two usernames match. In a real application, we might want more
functionality, like the ability to hash and compare usernames, but we've kept
this example purposefully simple.

The basic structure of a module declaration like this is:

<link rel="import" href="code/files-modules-and-programs/module.syntax" />

We could have written this slightly differently, by giving the signature its
own top-level `module type` declaration, making it possible to create
multiple distinct types with the same underlying implementation in a
lightweight way:

<link rel="import" href="code/files-modules-and-programs/session_info/session_info.ml" />

The preceding code has a bug: it compares the username in one session to the
host in the other session, when it should be comparing the usernames in both
cases. Because of how we defined our types, however, the compiler will flag
this bug for us.

<link rel="import" href="code/files-modules-and-programs/session_info/build_session_info.errsh" />

This is a trivial example, but confusing different kinds of identifiers is a
very real source of bugs, and the approach of minting abstract types for
different classes of identifiers is an effective way of avoiding such issues.

## Opening Modules {#opening-modules data-type=sect1}

Most of the time, you refer to values and types within a module by using the
module name as an explicit qualifier. For example, you write `List.map` to
refer to the `map` function in the `List` module. Sometimes, though, you want
to be able to refer to the contents of a module without this explicit
qualification. That's what the `open` statement is for. [identifiers/open
modules and]{.idx}[modules/opening]{.idx}

We've encountered `open` already, specifically where we've written
`open Base` to get access to the standard definitions in the `Base` library.
In general, opening a module adds the contents of that module to the
environment that the compiler looks at to find the definition of various
identifiers. Here's an example:

<link rel="import" href="code/files-modules-and-programs/main.mlt" part=
"0.5" />

`open` is essential when you want to modify your environment for a standard
library like `Base`, but it's generally good style to keep the opening of
modules to a minimum. Opening a module is basically a trade-off between
terseness and explicitness—the more modules you open, the fewer module
qualifications you need, and the harder it is to look at an identifier and
figure out where it comes from.

Here's some general advice on how to deal with `open`s: [local opens]{.idx}

- Opening modules at the toplevel of a module should be done quite sparingly,
  and generally only with modules that have been specifically designed to be
  opened, like `Base` or `Option.Monad_infix`.

- If you do need to do an open, it's better to do a *local open*. There are
  two syntaxes for local opens. For example, you can write:
  
  <link rel="import" href="code/files-modules-and-programs/main.mlt" part=
  "1" />
  
  Here, `of_int` and the infix operators are the ones from the `Int64`
  module.
  
  There's another, even more lightweight syntax for local `open`s, which is
  particularly useful for small expressions:
  
  <link rel="import" href="code/files-modules-and-programs/main.mlt" part=
  "2" />

- An alternative to local `open`s that makes your code terser without giving
  up on explicitness is to locally rebind the name of a module. So, when
  using the `Counter.median` type, instead of writing:

- <link rel="import" href="code/files-modules-and-programs/freq-median/use_median_1.ml" part=
  "1" />
  
  you could write:
  
  <link rel="import" href="code/files-modules-and-programs/freq-median/use_median_2.ml" part=
  "1" />
  
  Because the module name `C` only exists for a short scope, it's easy to
  read and remember what `C` stands for. Rebinding modules to very short
  names at the top level of your module is usually a mistake.

## Including Modules {#including-modules data-type=sect1}

While opening a module affects the environment used to search for
identifiers, *including* a module is a way of adding new identifiers to a
module proper. Consider the following simple module for representing a range
of integer values: [modules/including]{.idx}[identifiers/adding to
modules]{.idx}

<link rel="import" href="code/files-modules-and-programs/main.mlt" part=
"3" />

We can use the `include` directive to create a new, extended version of the
`Interval` module:

<link rel="import" href="code/files-modules-and-programs/main.mlt" part=
"4" />

The difference between `include` and `open` is that we've done more than
change how identifiers are searched for: we've changed what's in the module.
If we'd used `open`, we'd have gotten a quite different result:

<link rel="import" href="code/files-modules-and-programs/main.mlt" part=
"5" />

To consider a more realistic example, imagine you wanted to build an extended
version of the `List` module, where you've added some functionality not
present in the module as distributed in `Base`. That's a job for `include`.

<link rel="import" href="code/files-modules-and-programs/ext_list.ml" />

Now, how do we write an interface for this new module? It turns out that
`include` works on signatures as well, so we can pull essentially the same
trick to write our `mli`. The only issues is that we need to get our hands on
the signature for the `List` module. This can be done using `module type of`,
which computes a signature from a module:

<link rel="import" href="code/files-modules-and-programs/ext_list.mli" />

Note that the order of declarations in the `mli` does not need to match the
order of declarations in the `ml`. The order of declarations in the `ml`
mostly matters insofar as it affects which values are shadowed. If we wanted
to replace a function in `List` with a new function of the same name, the
declaration of that function in the `ml` would have to come after the
`include List` declaration.

We can now use `Ext_list` as a replacement for `List`. If we want to use
`Ext_list` in preference to `List` in our project, we can create a file of
common definitions:

<link rel="import" href="code/files-modules-and-programs/common.ml" />

And if we then put `open Common` after `open Base` at the top of each file in
our project, then references to `List` will automatically go to `Ext_list`
instead.

## Common Errors with Modules {#common-errors-with-modules data-type=sect1}

When OCaml compiles a program with an `ml` and an `mli`, it will complain if
it detects a mismatch between the two. Here are some of the common errors
you'll run into.

### Type Mismatches {#type-mismatches data-type=sect2}

The simplest kind of error is where the type specified in the signature does
not match the type in the implementation of the module. As an example, if we
replace the `val` declaration in `counter.mli` by swapping the types of the
first two arguments: [errors/module type mismatches]{.idx}[type
mismatches]{.idx}[modules/type mismatches in]{.idx}

<link rel="import" href="code/files-modules-and-programs/freq-with-sig-mismatch/counter.mli" part=
"1" />

and we try to compile, we'll get the following error.

<link rel="import" href="code/files-modules-and-programs/freq-with-sig-mismatch/build.errsh" />

### Missing Definitions {#missing-definitions data-type=sect2}

We might decide that we want a new function in `Counter` for pulling out the
frequency count of a given string. We can update the `mli` by adding the
following line: [errors/missing module definitions]{.idx}[modules/missing
definitions in]{.idx}

<link rel="import" href="code/files-modules-and-programs/freq-with-missing-def/counter.mli" part=
"1" />

Now if we try to compile without actually adding the implementation, we'll
get this error.

<link rel="import" href="code/files-modules-and-programs/freq-with-missing-def/build.errsh" />

A missing type definition will lead to a similar error.

### Type Definition Mismatches {#type-definition-mismatches data-type=sect2}

Type definitions that show up in an `mli` need to match up with corresponding
definitions in the `ml`. Consider again the example of the type `median`. The
order of the declaration of variants matters to the OCaml compiler, so the
definition of `median` in the implementation listing those options in a
different order: [type definition mismatches]{.idx}[errors/module type
definition mismatches]{.idx}[modules/type definition mismatches]{.idx}

<link rel="import" href="code/files-modules-and-programs/freq-with-type-mismatch/counter.mli" part=
"1" />

will lead to a compilation error.

<link rel="import" href="code/files-modules-and-programs/freq-with-type-mismatch/build.errsh" />

Order is similarly important to other type declarations, including the order
in which record fields are declared and the order of arguments (including
labeled and optional arguments) to a function.

### Cyclic Dependencies {#cyclic-dependencies data-type=sect2}

In most cases, OCaml doesn't allow cyclic dependencies, i.e., a collection of
definitions that all refer to one another. If you want to create such
definitions, you typically have to mark them specially. For example, when
defining a set of mutually recursive values (like the definition of `is_even`
and `is_odd` in
[Recursive Functions](variables-and-functions.html#recursive-functions){data-type=xref}),
you need to define them using `let rec` rather than ordinary `let`.
[dependencies, cyclic]{.idx}[cyclic dependencies]{.idx}[errors/cyclic
dependencies]{.idx}[modules/cyclic dependencies]{.idx}

The same is true at the module level. By default, cyclic dependencies between
modules are not allowed, and cyclic dependencies among files are never
allowed. Recursive modules are possible but are a rare case, and we won't
discuss them further here.

The simplest example of a forbidden circular reference is a module referring
to its own module name. So, if we tried to add a reference to `Counter` from
within `counter.ml`.

<link rel="import" href="code/files-modules-and-programs/freq-cyclic1/counter.ml" part=
"1" />

we'll see this error when we try to build:

<link rel="import" href="code/files-modules-and-programs/freq-cyclic1/build.errsh" />

The problem manifests in a different way if we create cyclic references
between files. We could create such a situation by adding a reference to
`Freq` from `counter.ml`, e.g., by adding the following line.

<link rel="import" href="code/files-modules-and-programs/freq-cyclic2/counter.ml" part=
"1" />

In this case, `jbuilder` will notice the error and complain explicitly about
the cycle:

<link rel="import" href="code/files-modules-and-programs/freq-cyclic2/build.errsh" />


## Designing with Modules {#designing-with-modules data-type=sect1}

The module system is a key part of how an OCaml program is structured. As
such, we'll close this chapter with some advice on how to think about
designing that structure effectively.

### Expose Concrete Types Rarely {#expose-concrete-types-rarely data-type=sect2}

When designing an `mli`, one choice that you need to make is whether to
expose the concrete definition of your types or leave them abstract. Most of
the time, abstraction is the right choice, for two reasons: it enhances the
flexibility of your design, and it makes it possible to enforce invariants on
the use of your module.

Abstraction enhances flexibility by restricting how users can interact with
your types, thus reducing the ways in which users can depend on the details
of your implementation. If you expose types explicitly, then users can depend
on any and every detail of the types you choose. If they're abstract, then
only the specific operations you want to expose are available. This means
that you can freely change the implementation without affecting clients, as
long as you preserve the semantics of those operations.

In a similar way, abstraction allows you to enforce invariants on your types.
If your types are exposed, then users of the module can create new instances
of that type (or if mutable, modify existing instances) in any way allowed by
the underlying type. That may violate a desired invariant *i.e.*, a property
about your type that is always supposed to be true. Abstract types allow you
to protect invariants by making sure that you only expose functions that
preserves your invariants.

Despite these benefits, there is a trade-off here. In particular, exposing
types concretely makes it possible to use pattern-matching with those types,
which as we saw in <span class="keep-together">Lists And Patterns</span> is a
powerful and important tool. You should generally only expose the concrete
implementation of your types when there's significant value in the ability to
pattern match, and when the invariants that you care about are already
enforced by the data type itself.

### Design for the Call Site {#design-for-the-call-site data-type=sect2}

When writing an interface, you should think not just about how easy it is to
understand the interface for someone who reads your carefully documented
`mli` file, but more importantly, you want the call to be as obvious as
possible for someone who is reading it at the call site.

The reason for this is that most of the time, people interacting with your
API will be doing so by reading and modifying code that uses the API, not by
reading the interface definition. By making your API as obvious as possible
from that perspective, you simplify the lives of your users.

There are many ways of improving readability at the call site. One example is
labeled arguments (discussed in
[Labeled Arguments](variables-and-functions.html#labeled-arguments){data-type=xref}),
which act as documentation that is available at the call site.

You can also improve readability simply by choosing good names for your
functions, variant tags and record fields. Good names aren't always long, to
be clear. If you wanted to write an anonymous function for doubling a number:
`(fun x -> x * 2)`, a short variable name like `x` is best. A good rule of
thumb is that names that have a small scope should be short, whereas names
that have a large scope, like the name of a function in an a module
interface, should be longer and more explicit.

There is of course a tradeoff here, in that making your APIs more explicit
tends to make them more verbose as well. Another useful rule of thumb is that
more rarely used names should be longer and more explicit, since the cost of
concision and the benefit of explicitness become more important the more
often a name is used.

### Create Uniform Interfaces {#create-uniform-intefaces data-type=sect2}

Designing the interface of a module is a task that should not be thought of
in isolation. The interfaces that appear in your codebase should play
together harmoniously. Part of achieving that is standardizing aspects of
those interfaces.

`Base`, `Core` and other libraries from the same family have been designed
with a uniform set of standards in mind around the design of module
interfaces. Here are some of the guidelines that they use.

- *A module for (almost) every type.* You should mint a module for almost
  every type in your program, and the primary type of a given module should
  be called `t`.

- *Put `t` first*. If you have a module `M` whose primary type is `M.t`, the
  functions in `M` that take a value of `M.t` should take it as their first
  argument.

- Functions that routinely throw an exception should end in `_exn`.
  Otherwise, errors should be signaled by returning an `option` or an
  `Or_error.t` (both of which are discussed in
  [Error Handling](error-handling.html#error-handling){data-type=xref} ).

There are also standards in Base about what the type signature for specific
functions should be. For example, the signature for `map` is always
essentially the same, no matter what the underlying type it is applied to.
This kind of function-by-function API uniformity is achieved through the use
of *signature includes*, which allow for different modules to share
components of their interface. This approach is described in
[Using Multiple Interfaces](functors.html#using-multiple-interfaces){data-type=xref}.

Base's standards may or may not fit your projects, but you can improve the
usability of your codebase by finding some consistent set of standards to
apply.

### Interfaces before implementations {#interfaces-before-implementations data-type=sect2}

OCaml's concise and flexible type language enables a type-oriented approach
to software design. Such an approach involves thinking through and writing
out the types you're going to use before embarking on the implementation
itself.

This is a good approach both when working in the core language, where you
would write your type definitions before writing the logic of your
computations, as well as at the module level, where you would write a first
draft of your `mli` before working on the `ml`.

Of course, the design process goes in both directions. You'll often find
yourself going back and modifying your types in response to things you learn
by working on the implementation. But types and signatures provide a
lightweight tool for constructing a skeleton of your design in a way that
helps clarify your goals and intent, before you spend a lot of time and
effort fleshing it out.



