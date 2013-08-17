# Foreign Function Interface

OCaml has several options available to interact with non-OCaml code.  The
compiler can link with external system libraries via C code and also produce
standalone native object files that can be embedded within other non-OCaml
applications.

The mechanism by which code in one programming language can invoke routines in
another different programming language is called a *foreign function
interface*.  This chapter will:

* show how to call routines in C libraries directly from your OCaml code.
* teach you how to build higher-level abstractions in OCaml from the low-level C bindings.
* work through some full examples for binding a terminal interface and UNIX date/time functions.

The simplest foreign function interface in OCaml doesn't even require you to
write any C code at all!  The Ctypes library lets you define the C interface in
pure OCaml, and the library then takes care of loading the C symbols and
invoking the foreign function call.

Let's dive straight into a realistic example to show you how the library looks.
We'll create a binding to the Ncurses terminal toolkit, as it's widely
available on most systems and doesn't have any complex dependencies.

<note>
<title>Installing the Ctypes library</title>

You'll need to install the [`libffi`](https://github.com/atgreen/libffi)
library as a prerequisite to using Ctypes. It's a fairly popular library and
should be available in your OS package manager.

A special note for Mac users: the version of `libffi` installed by default in
MacOS X 10.8 is too old for some of the features that Ctypes needs.  Use
Homebrew to `brew install libffi` to get the latest version before installing
the OCaml library.

Once that's done, Ctypes is available via OPAM as usual.

```frag
((typ console)(name ffi/install.out))
```

You'll also need the Ncurses library for the first example. This comes
pre-installed on many operating systems such as MacOS X and Debian Linux
provides it as the `ncurses-dev` package.

</note>

## Example: a terminal interface

Ncurses is a library to help build terminal-independent text interfaces in a
reasonably efficient way.  It's used in console mail clients like Mutt and
Pine, and console web browsers such as Lynx.

The full C interface is quite large and is explained in the online
[documentation](http://www.gnu.org/software/ncurses/).  We'll just use the
small excerpt that's shown below since we just want to demonstrate Ctypes in
action.

```frag
((typ c)(name ffi/ncurses.h))
```

The Ncurses functions either operate on the current pseudo-terminal or on a
window that has been created via `newwin`.  The `WINDOW` structure holds the
internal library state and is considered abstract outside of Ncurses.  Ncurses
clients just need to store the pointer somewhere and pass it back to Ncurses
library calls, which in turn dereference its contents.

Note that there are over 200 library calls in Ncurses, so we're only binding a
select few for this example. The `initscr` and `newwin` create `WINDOW`
pointers for the global and sub-windows respectively.  The `mvwaddrstr` takes a
window, x/y offsets and a string and writes to the screen at that location.
The terminal is only updated after `refresh` or `wrefresh` are called. 

Ctypes provides an OCaml interface that lets you map these C functions to
equivalent OCaml functions.  The library takes care of converting OCaml
function calls and arguments into the C calling convention, invoking the
foreign call within the C library and finally returning the result as an OCaml
value.

Let's begin by defining the basic values we need, starting with the `WINDOW`
state pointer.

```frag
((typ ocaml)(name ffi/ncurses.ml)(part 0))
```

We don't know the internal representation of the window pointer, so we treat it
as a C void pointer.  We'll improve on this later on in the chapter, but that's
good enough for now.  The second statement defines an OCaml value that
represents the `WINDOW` C pointer.  This value is used later in the Ctypes
function definitions.

```frag
((typ ocaml)(name ffi/ncurses.ml)(part 1))
```

That's all we need to invoke our first function call to `initscr` to initialize
the terminal.  The `foreign` function accepts two parameters:

- the C function call name, which is looked up using the `dlsym` POSIX function.
- a value that defines the complete set of C function arguments and its return type.
  The `@->` operator adds an argument to the C parameter list and `returning`
  terminates the parameter list with the return type.

The remainder of the Ncurses binding simply expands on these definitions.

```frag
((typ ocaml)(name ffi/ncurses.ml)(part 2))
```

These definitions are all straightforward mappings from the C declarations in
the Ncurses header file.  Note that the `string` and `int` values here are
nothing to do with OCaml type declarations; instead, they are values that come
from opening the `Ctypes` module at the top of the file.

Most of the parameters in the Ncurses example represent fairly simple scalar C
types except for `window` (a pointer to the library state), and `string`, which
maps from OCaml strings that have a specific length onto C character buffers
whose length is defined by a terminating null character that immediately
follows the string data.

The module signature for `ncurses.mli` looks much like a normal OCaml
signature. You can infer it directly from the `ncurses.ml` by running a special
build target.

```frag
((typ console)(name ffi/infer_ncurses.out))
```

The `inferred.mli` target instructs the compiler to generate the default
signature for a module file, and places it in the `_build` directory as a
normal output.  You should normally copy it out into your source directory and
customize it improve its safety for external callers by making some of its
internals more abstract.

Here's the customized interface that we can safely use from other libraries.

```frag
((typ ocaml)(name ffi/ncurses.mli))
```

The `window` type is left abstract in the signature to ensure that window
pointers can only be constructed via the `Ncurses.initscr` function.  This
prevents void pointers obtained from other sources from being mistakenly passed
to an Ncurses library call.

Now compile a "hello world" terminal drawing program to tie this all together.

```frag
((typ ocaml)(name ffi/hello.ml))
```

The `hello` executable is compiled by linking with the `ctypes.foreign`
OCamlfind package.

```frag
((typ console)(name ffi/build_hello.out))
```

Running `./hello.native` should now display a Hello World in your terminal!

<note>
<title>On build directives for Ctypes</title>

The command-line above include some important extra link directives.  The
`-lflags` instructs <command>ocamlbuild</command> to pass the next
comma-separated set of arguments through to the <command>ocaml</command>
command when linking a binary.  OCaml in turn uses `-cclib` to pass directives
through to the system compiler (normally <command>gcc</command> or <command>clang</command).  We first need to
link to the `ncurses` C library to make the symbols available to Ctypes, and
`-cclib,-lncurses` does that.

On some distributions such as Ubuntu 11.10 upwards, you'll also need to add
`-cclib,-Xlinker,-cclib,--no-as-needed` to the `-lflags` directive.  `-Xlinker`
is interpreted by the compiler as a directive for the system linker <command>ld</command>, to
which it passes `--no-as-needed`.  Several modern OS distributions (such as
Ubuntu 11.10 onwards) configure the system linker to only link in libraries
that directly contain symbols used by the program.  However, when we use
Ctypes, those symbols are not referenced until runtime, which results an
exception due to the library not being available.

The `--no-as-needed` flag disables this behavior and ensures all the specified
libraries are linked despite not being directly used.  The flag unfortunately
doesn't work everywhere (notably, MacOS X should _not_ have this passed to it).

</note>

Ctypes wouldn't be very useful if it were limited to only defining simple C
types of course. It provides full support for C pointer arithmetic, pointer
conversions, reading and writing through pointers, using OCaml functions as
function pointers to C code, as well as struct and union definitions.

We'll go over some of these features in more detail for the remainder of the
chapter by using some POSIX date functions as running examples.

## Basic scalar C types

First, let's look at how to define basic scalar C types.  Every C type is
represented by an OCaml equivalent via the single type definition below.

```frag
((typ ocaml)(name ctypes/ctypes.mli)(part 0))
```

`Ctypes.typ` is the type of values that represents C types to OCaml.  There are
two types associated with each instance of `typ`:

* the C type used to store and pass values to the foreign library.
* the corresponding OCaml type.  The `'a` type parameter contains the OCaml type
  such that a value of type `t typ` is used to read and write OCaml values of type `t`.

There are various other uses of `typ` values within Ctypes, such as:

* constructing function types for binding native functions.
* constructing pointers for reading and writing locations in C-managed storage.
* describing component fields of structures, unions and arrays.

Here are the definitions for most of the standard C99 scalar types, including
some platform-dependent ones.

```frag
((typ ocaml)(name ctypes/ctypes.mli)(part 1))
```

These values are all of type `'a typ`, where the value name (_e.g._ `void`)
tells you the C type and the `'a` component (_e.g._ `unit`) is the OCaml
representation of that C type.  Most of the mappings are straightforward, but
some of them need a bit more explanation.

* Void values appear in OCaml as the `unit` type. Using `void` in an
  argument or result type specification produces an OCaml function which accepts or returns unit.
  Dereferencing a pointer to `void` is an error, as in C, and will raise the `IncompleteType` exception.
* The C `size_t` type is an alias for one of the unsigned integer types.  The actual 
  size and alignment requirements for `size_t` varies between platforms. Ctypes provides
  an OCaml `size_t` type that is aliased to the appropriate integer type.
* OCaml only supports double-precision floating-point numbers, and so the C
  `float` and `double` types both map onto the OCaml `float` type, and the C
  `float complex` and `double complex` types both map onto the OCaml
  double-precision `Complex.t` type.

## Pointers and arrays

Pointers are at the heart of C, so they are necessarily part of Ctypes, which
provides support for pointer arithmetic, pointer conversions, reading and
writing through pointers, and passing and returning pointers to and from
functions.

We've already seen a simple use of pointers in the Ncurses example.  Let's
start a new example by binding the following POSIX functions.

```frag
((typ c)(name ffi/posix_headers.h))
```

The `time` function returns the current calendar time and is a simple start.
The first step is to open some of the Ctypes modules.

* The `Ctypes` module provides functions for describing C types in OCaml.
* The `PosixTypes` module includes some extra POSIX-specific types (such as `time_t`).
* The `Foreign` module exposes the `foreign` function that makes it possible to invoke C functions.

We can now create a binding to `time` directly from the top-level.

```frag
((typ ocamltop)(name ffi/posix.topscript)(part 0))
```

The `foreign` function is the main link between OCaml and C.  It takes two
arguments: the name of the C function to bind, and a value describing the type
of the bound function.  In the `time` binding, the function type specifies one
argument of type `ptr time_t` and a return type of `time_t`.

We can now call `time` immediately in the same top-level.  The argument is
actually optional, so we'll just pass a null pointer that has been coerced into
becoming a null pointer to `time_t`.

```frag
((typ ocamltop)(name ffi/posix.topscript)(part 1))
```

Since we're going to call `time` a few times, let's create a wrapper function
that passes the null pointer through.

```frag
((typ ocamltop)(name ffi/posix.topscript)(part 2))
```

Since `time_t` is an abstract type, we can't actually do anything useful with
it directly. We need to bind a second function to do anything useful with the
return values from `time`.  We'll move on to `difftime`; the second C function
in our prototype list above.

```frag
((typ ocamltop)(name ffi/posix.topscript)(part 3))
```

The binding to `difftime` above is sufficient to compare two `time_t` values.

### Allocating typed memory for pointers

Let's look at a slightly less trivial example where we pass a non-null pointer
to a function.  Continuing with the theme from earlier, we'll bind to the
`ctime` function which converts a `time_t` value to a human-readable string.

```frag
((typ ocamltop)(name ffi/posix.topscript)(part 4))
```

The binding is continued in the top-level to add to our growing collection.
However, we can't just pass the result of `time` to `ctime`.

```frag
((typ ocamltop)(name ffi/posix.topscript)(part 5))
```

This is because `ctime` needs a pointer to the `time_t` rather than passing it
by value.  We thus need to allocate some memory for the `time_t` and obtain its
memory address.

```frag
((typ ocamltop)(name ffi/posix.topscript)(part 6))
```

The `allocate` function takes the type of the memory to be allocated and the
initial value, and it returns a suitably-typed pointer.  We can now call
`ctime` passing the pointer as an argument:

```frag
((typ ocamltop)(name ffi/posix.topscript)(part 7))
```

### Using views to map complex values

While scalar types typically have a 1:1 representation, other C types require
extra work to convert them into OCaml. Views create new C type descriptions
that have special behavior when used to read or write C values.

We've already used one view in the definition of `ctime` earlier. The `string`
view wraps the C type `char *` (written in OCaml as `ptr char`), and converts
between the C and OCaml string representations each time the value is written
or read.

Here is the type signature of the `Ctypes.view` function.

```frag
((typ ocaml)(name ctypes/ctypes.mli)(part 2))
```

Ctypes has some internal low-level conversion functions that map between an
OCaml `string` and a C character buffer by copying the contents into the
respective data structure.  They have the following type signature.

```frag
((typ ocaml)(name ctypes/ctypes.mli)(part 3))
```

Given these functions, the definition of the `Ctypes.string` value that uses
views is quite simple.

```frag
((typ ocaml)(name ctypes/ctypes_impl.ml)(part 0))
```

The type of this `string` function is a normal `typ` with no external sign of
the use of the view function.

```frag
((typ ocaml)(name ctypes/ctypes.mli)(part 4))
```

<note>
<title>OCaml strings versus C character buffers</title>

Although OCaml strings may look like C character buffers from an interface
perspective, they're very different in terms of their memory representations.

OCaml strings are stored in the OCaml heap with a header that explicitly
defines their length.  C buffers are also fixed-length, but by convention a C
string is terminated by a null (a `\0` byte) character.  The C string functions
calculate their length by scanning the buffer until the first null character is
encountered.

This means that you need to be careful that OCaml strings that you pass to C
functions don't contain any null values, since the first occurrence of a null
character will be treated as the end of the C string.  Ctypes also defaults to a
*copying* interface for strings, which means that you shouldn't use them when
you want the library to mutate the buffer in-place.  In that situation, use the
Ctypes `Bigarray` support to pass memory by reference instead.

</note>

## Structs and unions

The C constructs `struct` and `union` make it possible to build new types from
existing types.  Ctypes contains counterparts that work similarly.

### Defining a structure

Let's improve the timer function that we wrote earlier.  The POSIX function
`gettimeofday` retrieves the time with microsecond resolution.  The signature
of `gettimeofday` is as follows, including the structure definitions.

```frag
((typ c)(name ffi/timeval_headers.h))
```

Using Ctypes, we can describe this type as follows in our top-level, continuing
on from the previous definitions.

```frag
((typ ocamltop)(name ffi/posix.topscript)(part 8))
```

The first command defines a new OCaml type `timeval` that we'll use to
instantiate the OCaml version of the `struct`. Creating a new OCaml type to
reflect the underlying C type in this way means that the structure we define
will be distinct from other structures we define elsewhere, which helps to
avoid getting them mixed up.

The second command calls `structure` to create a fresh structure type.  At this
point the structure type is incomplete: we can add fields but cannot yet use it
in `foreign` calls or use it to create values.

### Adding fields to structures

The `timeval` structure definition still doesn't have any fields, so we need to
add those next.

```frag
((typ ocamltop)(name ffi/posix.topscript)(part 9))
```

The `field` function appends a field to the structure, as shown with `tv_sec`
and `tv_usec` above.  Structure fields are typed accessors that are associated
with a particular structure, and they correspond to the labels in C.

Every field addition mutates the structure variable and records a new size (the
exact value of which depends on the type of the field that was just added).
Once we `seal` the structure we will be able to create values using it, but
adding fields to a sealed structure is an error.

### Incomplete structure definitions

Since `gettimeofday` needs a `struct timezone` pointer for its second
argument, we also need to define a second structure type.

```frag
((typ ocamltop)(name ffi/posix.topscript)(part 10))
```

We don't ever need to create `struct timezone` values, so we can leave this
struct as incomplete without adding any fields or sealing it.  If you ever try
to use it in a situation where its concrete size needs to be known, the library
will raise an `IncompleteType` exception.

We're finally ready to bind to `gettimeofday` now.

```frag
((typ ocamltop)(name ffi/posix.topscript)(part 11))
```

There's one other new feature here: the `returning_checking_errno` function
behaves like `returning`, except that it checks whether the bound C function
modifies the C error flag.  Changes to `errno` are mapped into OCaml exceptions
and raise a `Unix.Unix_error` exception just as the standard library functions
do.

As before we can create a wrapper to make `gettimeofday` easier to use.  The
functions `make`, `addr` and `getf` create a structure value, retrieve the
address of a structure value, and retrieve the value of a field from a
structure.

```frag
((typ ocamltop)(name ffi/posix.topscript)(part 12))
```

You need to be a little careful not to get all the open modules mixed up here.
Both `Pervasives` and `Ctypes` define different `float` functions.  The
`Ctypes` module we opened up earlier overrides the `Pervasives` definition.  As
seen above though, you just need to locally open `Pervasives` again to bring
the usual `float` function back in scope,

#### Recap: a time-printing command

We built up a lot of bindings in the earlier section, so let's recap them
with a complete example that ties it together with a command-line frontend.

```frag
((typ ocaml)(name ffi/datetime.ml))
```

This can be compiled and run in the usual way.

```frag
((typ ocaml)(name ffi/build_datetime.out))
```

<sidebar>
<title>Why do we need to use `returning`?</title>

The alert reader may be curious why all these function definitions have to be
terminated by `returning`.

```frag
((typ ocaml)(name ffi/return_frag.ml)(part 0))
```

The `returning` function may appear superfluous here. Why couldn't we simply
give the types as follows?

```frag
((typ ocaml)(name ffi/return_frag.ml)(part 1))
```

The reason involves higher types and two differences between the way that
functions are treated in OCaml and C.
Functions are first-class values in OCaml, but not in C. For example, in C,
it is possible to return a function pointer from a function, but not to return
an actual function.

Secondly, OCaml functions are typically defined in a curried style. The signature of
a two-argument function is written as follows:

```frag
((typ ocaml)(name ffi/return_frag.ml)(part 2))
```

but this really means

```frag
((typ ocaml)(name ffi/return_frag.ml)(part 3))
```

and the arguments can be supplied one at a time to create a closure.  In
contrast, C functions receive their arguments all at once.  The equivalent C
function type is the following:

```frag
((typ c)(name ffi/return_c_frag.h))
```

and the arguments must always be supplied together:

```frag
((typ c)(name ffi/return_c_frag.c))
```

A C function that's written in curried style looks very different:

```frag
((typ c)(name ffi/return_c_uncurried.c))
```

The OCaml type of `uncurried_C` when bound by Ctypes is `int -> int -> int`: a
two-argument function.  The OCaml type of `curried_C` when bound by `ctypes` is
`int -> (int -> int)`: a one-argument function that returns a one-argument
function.

In OCaml, of course, these types are absolutely equivalent.  Since the OCaml
types are the same but the C semantics are quite different, we need some kind
of marker to distinguish the cases.  This is the purpose of `returning` in
function definitions.

</sidebar>

### Defining arrays

Arrays in C are contiguous blocks of the same type of value.  Any of the basic
types defined earlier can be allocated as blocks via the `Array` module.

```frag
((typ ocaml)(name ctypes/ctypes.mli)(part 5))
```

The array functions are similar to those in the standard library `Array`
module except that they operate on arrays stored using the flat C
representation rather than the OCaml representation described in
[xref](#memory-representation-of-values).

As with standard OCaml arrays, the conversion between arrays and lists
requires copying the values, which can be expensive for large data structures.
Notice that you can also convert an array into a `ptr` pointer to the head of
the underlying buffer, which can be useful if you need to pass the pointer and
size arguments separately to a C function.

Unions in C are named structures that can be mapped onto the same underlying
memory.  They are also fully supported in Ctypes, but we won't go into more
detail here.

<sidebar>
<title>Pointer operators for dereferencing and arithmetic</title>

Ctypes defines a number of operators that let you manipulate pointers and arrays
just as you would in C.  The Ctypes equivalents do have the benefit of being
more strongly typed, of course.

Operator    Purpose
--------    -------
`!@ p`      Dereference the pointer `p`.
`p <-@ v`   Write the value `v` to the address `p`.
`p +@ n`    If `p` points to an array element, then compute the address of the `n`th next element.
`p -@ n`    If `p` points to an array element, then compute the address of the `n`th previous element.

There are also other useful non-operator functions available (see the Ctypes
documentation), for example for pointer differencing and comparison.

</sidebar>

## Passing functions to C

It's also straightforward to pass OCaml function values to C.  The C standard
library function `qsort` sorts arrays of elements using a comparison function
passed in as a function pointer.  The signature for `qsort` is as follows:

```frag
((typ c)(name ffi/qsort.h))
```

C programmers often use `typedef` to make type definitions involving function
pointers easier to read.  Using a typedef, the type of `qsort` looks a little
more palatable.

```frag
((typ c)(name ffi/qsort_typedef.h))
```

This also happens to be a close mapping to the corresponding Ctypes definition.
Since type descriptions are regular values, we can just use `let` in place of
`typedef` and end up with working OCaml bindings to `qsort`.

```frag
((typ ocamltop)(name ffi/qsort.topscript))
```

We only use `compare_t` once (in the `qsort` definition), so you can choose to
inline it in the OCaml code if you prefer. As the type shows, the resulting
`qsort` value is a higher-order function, since its the fourth argument is
itself a function.  As before, let's define a wrapper function to make `qsort`
easier to use.  The second and third arguments to `qsort` specify the length
(number of elements) of the array and the element size.

Arrays created using Ctypes have a richer runtime structure than C arrays, so
we don't need to pass size information around.  Furthermore, we can use OCaml
polymorphism in place of the unsafe `void ptr` type.

### Example: a command-line quicksort

Below is a command-line tool that uses the `qsort` binding to sort all of
the integers supplied on the standard input.

```frag
((typ ocaml)(name ffi/qsort.ml))
```

Compile it in the usual way with corebuild and test it against some input
data, and also build the inferred interface so we can examine it more closely.

```frag
((typ console)(name ffi/build_qsort.out))
```

The inferred interface shows us the types of the raw `qsort` binding and
also the `qsort'` wrapper function.

```frag
((typ ocaml)(name ffi/qsort.mli))
```

The `qsort'` wrapper function has a much more canonical OCaml interface than
the raw binding.  It accepts a comparator function and a Ctypes array, and
returns the same Ctypes array.  It's not strictly required that it returns the
array since it modifies it in-place, but it makes it easier to chain the
function using the `|>` operator (as `sort_stdin` does in the example).

Using `qsort'` to sort arrays is straightforward.  Our example code reads the
standard input as a list, converts it to a C array, passes it through qsort,
and outputs the result to the standard output.  Again, remember to not confuse
the `Ctypes.Array` module with the `Core.Std.Array` module: the former is in
scope since we opened `Ctypes` at the start of the file.

<note>
<title>Lifetime of allocated Ctypes</title>

Values allocated via Ctypes (_i.e._ using `allocate`, `Array.make` and so on)
will not be garbage-collected as long as they are reachable from OCaml values.
The system memory they occupy is freed when they do become unreachable, via a
finalizer function registered with the GC.

The definition of reachability for Ctypes values is a little different from
conventional OCaml values, though.  The allocation functions return an
OCaml-managed pointer to the value, and as long as some derivative pointer is
still reachable by the GC, the value won't be collected.

"Derivative" means a pointer that's computed from the original pointer via
arithmetic, so a reachable reference to an array element or a structure field
protects the whole object from collection.

A corollary of the above rule is that pointers written into the C heap don't
have any effect on reachability.  For example, if you have a C-managed array of
pointers to structs then you'll need some additional way of keeping the structs
themselves around to protect them from collection.  You could achieve this via a
global array of values on the OCaml side that would keep them live until they're
no longer needed.

Functions passed to C have similar considerations regarding lifetime.  On the
OCaml side, functions created at runtime may be collected when they become
unreachable.  As we've seen, OCaml functions passed to C are converted to
function pointers, and function pointers written into the C heap have no effect
on the reachability of the OCaml functions they reference.  With `qsort` things
are straightforward, since the comparison function is only used during the call
to `qsort` itself.  However, other C libraries may store function pointers in
global variables or elsewhere, in which case you'll need to take care that the
OCaml functions you pass to them aren't prematurely garbage collected.

</note>

## Learning more about C bindings

The Ctypes [distribution](http://github.com/ocamllabs/ocaml-ctypes) contains a
number of larger-scale examples, including:

* bindings to the POSIX `fts` API which demonstrates C callbacks more comprehensively.
* a more complete Ncurses binding than the example we opened the chapter with.
* a comprehensive test suite that covers the complete library, and can provide useful snippets for your own bindings.

This chapter hasn't really needed you to understand the innards of OCaml at
all.  Ctypes does its best to make function bindings easy, but the rest of this
part will also fill you in about how interactions with OCaml memory layout in
[xref](#memory-representation-of-values) and automatic memory management in
[xref](#understanding-the-garbage-collector).

Ctypes gives OCaml programs access to the C representation of values, shielding
you from the details of the OCaml value representation, and introduces an
abstraction layer that hides the details of foreign calls.  While this covers a
wide variety of situations, it's sometimes necessary to look behind the
abstraction to obtain finer control over the details of the interaction between
the two languages.

You can find more information about the C interface in several places.

*   The standard OCaml foreign function interface allows you to
    glue OCaml and C together from the other side of the boundary, by writing C
    functions that operate on the OCaml representation of values.
    You can find details of the standard interface in the [OCaml manual](http://caml.inria.fr/pub/docs/manual-ocaml-4.00/manual033.html) and in
    the book [Developing Applications with Objective Caml](http://caml.inria.fr/pub/docs/oreilly-book/ocaml-ora-book.pdf).

*   Florent Monnier maintains an excellent online [reference](http://www.linux-nantes.org/~fmonnier/ocaml/ocaml-wrapping-c.php) that
    provides examples of how to call OCaml functions from C.  This covers a wide variety of OCaml data types and also
    more complex callbacks between C and OCaml.

*   [SWIG](http://www.swig.org) is a tool that connects programs written in
    C/C++ to a variety of higher-level programming languages, including OCaml.
    The SWIG manual has examples of converting library specifications into
    OCaml bindings.

### Struct memory layout

The C language gives implementations a certain amount of freedom in choosing
how to lay out structs in memory.  There may be padding between members and at
the end of the struct, in order to satisfy the memory alignment requirements of
the host platform.  Ctypes uses platform-appropriate size and alignment
information to replicate the struct layout process.  OCaml and C will have
consistent views abotu the layout of the struct as long as you declare the
fields of a struct in the same order and with the same types as the C library
you're binding to.

However, this approach can lead to difficulties when the fields of a struct
aren't fully specified in the interface of a library.  The interface may list
the fields of a structure without specifying their order, or make certain
fields available only on certain platforms, or insert undocumented fields into
struct definitions for performance reasons.  For example, the `struct timeval`
definition used in this chapter accurately describes the layout of the struct
on common platforms but implementations on some more unusual architectures
include additional padding members that will lead to strange behaviour in the
examples.

The Cstubs subpackage of Ctypes addresses this issue. Rather than simply
assuming that struct definitions given by the user accurately reflect the actual
definitions of structs used in C libraries, Cstubs generates code that uses the
C library headers to discover the layout of the struct.  The good news is that the
code that you write doesn't need to change much.  Cstubs provides alternative
implementations of the `field` and `seal` functions that you've already used to
describe `struct timeval`; instead of computing member offsets and sizes
appropriate for the platform, these implementations obtain them directly from C.

The details of using Cstubs are available in the online
[documentation](https://ocamllabs.github.io/ocaml-ctypes), along with
instructions on integration with <command>autoconf</command> platform
portability instructions.

<note>
<title>Production note</title>

This chapter contains significant contributions from Jeremy Yallop.

</note>
