# Foreign Function Interface

OCaml has several options available to interact with non-OCaml code.  The
compiler can link to external system libraries via C code, and also produce
standalone native object files that can be embedded within other non-OCaml
applications. 

## The Ctypes library

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

```
$ brew install libffi     # for MacOS X users
$ opam install ctypes
$ utop
# require "ctypes.foreign" ;;
```

You'll also need the Ncurses library for the first example. This comes
pre-installed on many operating systems such as MacOS X. Debian Linux provides
it as the `ncurses-dev` package.

</note>

## Example: a terminal interface

Ncurses is a library to help build terminal-independent text interfaces in a
reasonably efficient way.  It's used in console mail clients like Mutt and
Pine, and console web browsers such as Lynx.

The full C interface is quite large and is explained in the online
[documentation](http://www.gnu.org/software/ncurses/).  We'll just use the
smaller excerpt that's shown below since we just want to demonstrate Ctypes in
action.

```c
// <ncurses.h>
typedef struct _win_st WINDOW;

WINDOW *initscr   (void);
WINDOW *newwin    (int, int, int, int);
void    endwin    (void);
void    refresh   (void);
void    wrefresh  (WINDOW *);
void    mvwaddstr (WINDOW *, int, int, char *);
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

```ocaml
(* ncurses.ml 1/3 *)
open Ctypes

type window = unit ptr
let window : window typ = ptr void
```

We don't know the internal representation of the window pointer, so we treat it
as a C void pointer.  We'll improve on this later on in the chapter, but it's
good enough for now.  The second statement defines an OCaml value that
represents the `WINDOW` C pointer.  This value is used later in the Ctypes
function definitions.

That's all we need to invoke our first function call to `initscr` to initalize
the terminal.

```ocaml
(* ncurses.ml 2/3 *)
open Foreign

let initscr =
  foreign "initscr" (void @-> (returning window))
```

The `foreign` function accepts two parameters:

- the C function call name, which is looked up using *dlsym(3)*.
- a value that defines the complete set of C function arguments and its return type.
  The `@->` operator adds an argument to the C parameter list and `returning`
  terminates the parameter list with the return type.

The remainder of the Ncurses binding simply expands on these definitions.

```ocaml
(* ncurses.ml 3/3 *)
let endwin =
  foreign "endwin" (void @-> (returning void))

let refresh =
  foreign "refresh" (void @-> (returning void))

let wrefresh =
  foreign "wrefresh" (window @-> (returning void))

let newwin =
  foreign "newwin" 
    (int @-> int @-> int @-> int @-> (returning window))

let mvwaddch =
  foreign "mvwaddch" 
    (window @-> int @-> int @-> char @-> (returning void))

let addstr =
  foreign "addstr" (string @-> (returning void))

let mvwaddstr =
  foreign "mvwaddstr"
    (window @-> int @-> int @-> string @-> (returning void))

let box =
  foreign "box" (window @-> int @-> int @-> (returning void))

let cbreak =
  foreign "cbreak" (void @-> (returning void))
```

These definitions are all straightforward mappings from the C declarations in
the Ncurses header file.  The scalar C types such as `int` come pre-defined in
Ctypes. The `string` in these definitions maps from OCaml strings (which have a
specific length) onto C character buffers (whose length is defined by a null
characters).

The module signature for `ncurses.mli` looks much like a normal OCaml
signature. You can infer it directly from the `ncurses.ml` by running:

```console
$ ocamlfind ocamlc -i -package ctypes.foreign ncurses.ml
```

The OCaml signature can be customized to improve its safety for external
callers by making some of its internals more abstract.

```ocaml
(* ncurses.mli *)
type window

val window    : window Ctypes.typ
val initscr   : unit   -> window
val endwin    : unit   -> unit
val refresh   : unit   -> unit
val wrefresh  : window -> unit
val newwin    : int    -> int -> int -> int -> window
val mvwaddch  : window -> int -> int -> char -> unit
val addstr    : string -> unit
val mvwaddstr : window -> int -> int -> string -> unit
val box       : window -> int -> int -> unit
val cbreak    : unit   -> unit
```

The `window` type is left abstract in the signature, ensure that window
pointers can only be constructed via the `Ncurses.initscr` function.  This
prevents void pointers obtained from other sources from being mistakenly passed
to an Ncurses library call.

Now compile a "hello world" terminal drawing program to tie this all together.

```ocaml
(* hello.ml *)
open Ncurses

let () =
  let main_window = initscr () in
  cbreak ();
  let small_window = newwin 10 10 5 5 in
  mvwaddstr main_window 1 2 "Hello";
  mvwaddstr small_window 2 2 "World";
  box small_window 0 0;
  refresh ();
  Unix.sleep 1;
  wrefresh small_window;
  Unix.sleep 5;
  endwin ()
```

The `hello` executable is compiled by linking against the `ctypes.foreign`
OCamlfind package.

```console
$ ocamlfind ocamlopt -linkpkg -package ctypes.foreign -cclib -lncurses \
    ncurses.mli ncurses.ml hello.ml -o hello
```

Running `./hello` should now display a Hello World in your terminal!

The command-line above includes `-cclib -lncurses` to make the OCaml compiler
link the executable to the `ncurses` C library, which in turns makes the C
symbols available to the program when it starts.  You'll get an error when you
run the binary if you omit that link directive.

```console
$ ocamlfind ocamlopt -linkpkg -package ctypes -package unix \
  ncurses.mli ncurses.ml hello.ml -o hello_broken
$ ./hello_broken 
Fatal error: exception Dl.DL_error("dlsym(RTLD_DEFAULT, initscr): symbol not found")
```

Ctypes wouldn't be very useful if it were limited to only defining simple C
types of course. It provides full support for C pointer arithmetic, pointer
conversions, reading and writing through pointers, using OCaml functions as
function pointers to C code, as well as struct and union definitions.

We'll go over some of these features in more detail for the remainder of the
chapter, using some POSIX date functions as running examples.

## Basic scalar C types

First, let's look at how to define basic scalar C types.  Every C type is
represented by an OCaml equivalent via the single type definition below.

```ocaml
(* Ctypes *)
type 'a typ
```

`Ctypes.typ` is the type of values that represents C types to OCaml.  There are
two types associated with each instance of `typ`: 

* the C type used to store and pass values to the foreign library.
* the corresponding OCaml type.  The `'a` type parameter contains the OCaml type
  such that a value of type `t typ` is used to read and write OCaml values of type `t`.

There are various other uses of `typ` values within Ctypes.

* constructing function types for binding native functions.
* constructing pointers for reading and writing locations in C-managed storage.
* describing the fields of arrays, structures and unions.

Here are the definitions for most of the standard C99 scalar types, including
some platform-dependent ones.

```ocaml
(* Ctypes *)
val void  : unit typ
val char : char typ
val schar : int typ
val short : int typ
val int   : int typ
val long  : long typ
val llong  : llong typ
val nativeint : nativeint typ

val int8_t : int typ
val int16_t : int typ
val int32_t : int32 typ
val int64_t : int64 typ
val uchar : uchar typ
val uchar : uchar typ
val uint8_t : uint8 typ
val uint16_t : uint16 typ
val uint32_t : uint32 typ
val uint64_t : uint64 typ
val size_t : size_t typ
val ushort : ushort typ
val uint : uint typ
val ulong : ulong typ
val ullong : ullong typ

val float : float typ
val double : float typ
```

These values are all of type `'a typ`, where the value name (e.g. `void`) tells
you the C type and the `'a` component (e.g. `unit`) is the OCaml
representation of that C type.  Most of the mappings are straightforward, but
some of them need a bit more explanation.

* Void values appear in OCaml as the `unit` type. Using `void` in an
  argument or result type specification produces an OCaml function which accepts or returns unit.
  Dereferencing a pointer to `void` is an error, as in C, and will raise the `IncompleteType` exception.
* The C `size_t` type is an alias for one of the unsigned integer types.  The actual 
  size and alignment requirements for `size_t` varies between platforms. Ctypes provides
  an OCaml `size_t` type that is aliased to the appropriate integer type.
* OCaml only supports double-precision floating point numbers, and so the C `float` and
  `double` functions both map onto the OCaml `float` type.

## Pointers and arrays

Pointers are at the heart of C, so they are necessarily part of Ctypes, which
provides support for pointer arithmetic, pointer conversions, reading and
writing through pointers, and passing and returning pointers to and from
functions. 

We've already seen a simple use of pointers in the Ncurses example.  Let's
start a new example by binding some POSIX functions.  The `time` function
returns the current calendar time, and has the following C signature:

```c
time_t time(time_t *);
```
The first step is to open some of the Ctypes modules.

* The `Ctypes` module provides functions for describing C types in OCaml.
* The `PosixTypes` module includes some extra POSIX-specific types (such as `time_t`).
* The `Foreign` module exposes the `foreign` function that makes it possible to invoke C functions.

We can now create a binding to `time` directly from the top-level.

```ocaml
$ utop
# require "ctypes.foreign" ;;
# open Ctypes ;;
# open PosixTypes ;;
# open Foreign ;;
# let time = foreign "time" (ptr time_t @-> returning time_t) ;;
val time : time_t ptr -> time_t = <fun>
```

The `foreign` function is the main link between OCaml and C.  It takes two
arguments: the name of the C function to bind, and a value describing the type
of the bound function.  In the `time` binding, the function type specifies one
argument of type `ptr time_t` and a return type of `time_t`.

We can now call `time` immediately in the same top-level.  The argument is
actually optional, so we'll just pass a null pointer that has been coerced
into becoming a null pointer to `time_t`.

```ocaml
# let cur_time = time (from_voidp time_t null) ;;
val cur_time : time_t = <abstr>
```

Since we're going to call `time` a few times, let's create a wrapper function
that passes the null pointer through.

```ocaml
# let time' () = time (from_voidp time_t null) ;;
val time' : unit -> time_t = <fun>
```

Since `time_t` is an abstract type, we can't actually do anything useful with
it directly. We need to bind a second function to do anything useful with the
return values from `time`.  We'll use the standard C function `difftime`, which
has the following signature:

```c
double difftime(time_t, time_t);
```

A binding to `difftime` is sufficient to compare two `time_t` values.

```ocaml
# let difftime = foreign "difftime" (time_t @-> time_t @-> returning double) ;;
val difftime : time_t -> time_t -> float = <fun>
# let t1 = time' () in
  Unix.sleep 2;
  let t2 = time' () in 
  difftime t2 t1 ;;
 - : float = 2.
```

### Allocating typed memory for pointers

Let's look at a slightly less trivial example where we pass a non-null pointer
to a function.  Continuing with the theme from earlier, we'll bind to the
`ctime` function which converts a `time_t` value to a human-readable string.
The C signature of `ctime` is as follows:

```c
char *ctime(const time_t *timep);
```

The corresponding binding can be written in the top-level to add to our growing
collection.

```ocaml
# let ctime = foreign "ctime" (ptr time_t @-> returning string) ;;
val ctime : time_t ptr -> string = <fun>
```

However, we can't just pass the result of `time` to `ctime`.

```ocaml
# ctime (time' ());;
Error: This expression has type time_t but an expression was expected
of type time_t ptr
```

This is because `ctime` needs a pointer to the `time_t` rather than passing it
by value.  We thus need to allocate some memory for the `time_t` and obtain its
memory address.

``` ocaml
# let t_ptr = allocate time_t (time' ()) ;;
val t_ptr : time_t ptr = <abstr>
```

The `allocate` function takes the type of the memory to be allocated and the
initial value, and it returns a suitably-typed pointer.  We can now call
`ctime` passing the pointer as an argument:

```ocaml
# ctime t_ptr;;
- : string = "Sat Jun  8 12:20:42 2013\n"
```

### Using views to map complex values

While scalar types typically have a 1-1 representation, other C types require
extra work to convert them into OCaml. Views create new C type descriptions
that have special behaviour when used to read or write C values.

We've already used one view in the definition of `ctime` earlier. The `string`
view wraps the C type `char *` (written in OCaml as `ptr char`), and converts
between the C and OCaml string representations each time the value is written
or read.

<note>
<title>OCaml strings versus C character buffers</title>

Although OCaml strings may look like C character buffers from an interface
perspective, they're very different in terms of their memory representations.

OCaml strings are stored in the OCaml heap with a header that explicitly
defines its length.  C buffers are also fixed-length, but by convention a C
string is terminated by a null (a `0` byte) character.  The C string functions
calculate their length by scanning the buffer until the first null character is
encountered.

This means you need to be careful when passing OCaml strings to C buffers that
don't contain any null values within the OCaml string, or else the C
string will be rudely truncated.

</note>

Here are the type signatures of the view functions in Ctypes.

```ocaml
(* Ctypes *)
val view : read:('a -> 'b) -> write:('b -> 'a) -> 'a typ -> 'b typ
val string_of_char_ptr : char ptr -> string
val char_ptr_of_string : string -> char ptr
val string : string typ
```

The actual definition of `string` that uses views is quite simple and is
written using these functions.

```ocaml
let string = 
  view 
    ~read:string_of_char_ptr 
    ~write:char_ptr_of_string 
    (char ptr)
```

### Abstract pointers

Abstract types are typically used to interface with platform-dependent
definitions often found in system headers.  For example, the type `pthread_t`
is a pointer on some platforms, an integer on other platforms, and a `struct`
on a third set of platforms.  One way to deal with this is to have build-time
code which interrogates the C type in some way to determine an appropriate
representation.  Another way is to use `abstract` and leave the representation
opaque.

<caution>
<title>Abstract values can't be passed by value</title>

Although `pthread_t` is a convenient example since the type used to implement
it varies significantly across platforms, it's not actually a good match for
`abstract` since values of type `pthread_t` are passed and returned by value
and so can't be fully abstract.

</caution>

```ocaml   
val abstract : size:int -> alignment:int -> 'a abstract typ
```

The `abstract` function accepts size and alignment requirements and ensures
that these are satisfied when this type is used in a function call.

## Structs and unions

The C constructs `struct` and `union` make it possible to build new types from
existing types.  Ctypes contains counterparts that work similarly.

### Defining a structure

Let's improve the timer function that we wrote earlier.  The POSIX function
`gettimeofday` retrieves the time with microsecond resolution.  The signature
of `gettimeofday` is as follows, including the structure definitions.

```c
struct timeval {
  long tv_sec;
  long tv_usec;
};

int gettimeofday(struct timeval *, struct timezone *tv);
```

Using Ctypes, we can describe this type as follows in our top-level.

```ocaml
# type timeval;;
type timeval
# let timeval : timeval structure typ = structure "timeval" ;;
val timeval : timeval structure typ = <abstr>
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

```ocaml
# let tv_sec  = timeval *:* long ;;
val tv_sec : (Signed.long, timeval structure) field = <abstr>
# let tv_usec = timeval *:* long ;;
val tv_usec : (Signed.long, timeval structure) field = <abstr>
# seal timeval ;;
- : unit = ()
```

The `*:*` operator appends a field to the structure, as shown with `tv_sec` and
`tv_usec` above.  Structure fields are typed accessors that are associated with
a particular structure, and they correspond to the labels in C.  Note that
there's no explicit requirement that the OCaml variable names for a field are
the same as the corresponding C struct label names, but it helps avoid
confusion.

Every field addition mutates the structure variable and records a new size (the
exact value of which depends on the type of the field that was just added).
Once we `seal` the structure we will be able to create values using it, but
adding fields to a sealed structure is an error.

### Incomplete structure definitions

Since `gettimeofday` needs a `struct timezone` pointer for its second
argument, we also need to define a second structure type.

``` ocaml
# type timezone ;;
type timezone
# let timezone : timezone structure typ = structure "timezone" ;;
val timezone : timezone structure typ = <abstr>
```

We don't ever need to create `struct timezone` values, so we can leave this
struct as incomplete without adding any fields or sealing it.  If you ever try
to use it in a situation where its concrete size needs to be known, the library
will raise an `IncompleteType` exception.

We're finally ready to bind to `gettimeofday`.

```ocaml
# let gettimeofday = foreign "gettimeofday"
    (ptr timeval @-> ptr timezone @-> returning_checking_errno int) ;;
val gettimeofday : timeval structure ptr -> timezone structure ptr -> int = <fun>
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
 
``` ocaml
# let gettimeofday' () =
  let tv = make timeval in
  let _ = gettimeofday (addr tv) (from_voidp timezone null) in
  let secs = Signed.Long.(to_int (getf tv tv_sec)) in
  let usecs = Signed.Long.(to_int (getf tv tv_usec)) in
  Pervasives.(float secs +. float usecs /. 1000000.0) ;;
val gettimeofday : timeval structure ptr -> timezone structure ptr -> int = <fun>

# gettimeofday' () ;;
- : float = 1370714234.606070
```

You need to be a little careful not to get all the open modules mixed up here.
Both Pervasives and Ctypes define different `float` functions.  The Ctypes
module we opened up earlier overrides the Pervasives definition.  As seen above
though, you just need to locally open Pervasives again to bring the usual
`float` function back in scope,

#### Recap: a time-printing command

We built up a lot of bindings in the earlier section, so let's recap them
with a complete example that ties it together with a command-line frontend.

```ocaml
(* datetime.ml: display time in various formats *)
open Core.Std
open Ctypes
open PosixTypes
open Foreign

let time     = foreign "time" (ptr time_t @-> returning time_t)
let difftime = foreign "difftime" (time_t @-> time_t @-> returning double)
let ctime    = foreign "ctime" (ptr time_t @-> returning string)

type timeval
let timeval : timeval structure typ = structure "timeval"
let tv_sec   = timeval *:* long 
let tv_usec  = timeval *:* long 
let ()       = seal timeval

type timezone
let timezone : timezone structure typ = structure "timezone"

let gettimeofday = foreign "gettimeofday"
    (ptr timeval @-> ptr timezone @-> returning_checking_errno int)

let time' () = time (from_voidp time_t null)

let gettimeofday' () =
  let tv = make timeval in
  let _ = gettimeofday (addr tv) (from_voidp timezone null) in
  let secs = Signed.Long.(to_int (getf tv tv_sec)) in
  let usecs = Signed.Long.(to_int (getf tv tv_usec)) in
  Pervasives.(float secs +. float usecs /. 1_000_000.)

let float_time () = printf "%f%!\n" (gettimeofday' ())

let ascii_time () =
  let t_ptr = allocate time_t (time' ()) in
  printf "%s%!" (ctime t_ptr)

let () =
  let open Command in
  basic ~summary:"Display the current time in various formats"
    Spec.(empty +> flag "-a" no_arg ~doc:" Human-readable output format")
    (fun human -> if human then ascii_time else float_time)
  |> Command.run 
```

This can be compiled as usual with ocamlfind and ocamlopt.

```console
$ ocamlfind ocamlopt -o datetime -package core -package ctypes.foreign \
  -thread -linkpkg datetime.ml
$ ./datetime -a
Sat Jun  8 19:28:59 2013
```

<sidebar>
<title>Why do we need to use `returning`?</title>

The alert reader may be curious why all these function definitions have to be
terminated by `returning`.

```ocaml
val time: ptr time_t @-> returning time_t
val difftime: time_t @-> time_t @-> returning double
```

The `returning` function may appear superfluous here. Why couldn't we simply
give the types as follows?

``` ocaml
val time: ptr time_t @-> time_t
val difftime: time_t @-> time_t @-> double
```

The reason involves higher types and two differences between the way that
functions are treated in OCaml and C.
Functions are first-class values in OCaml, but not in C. For example, in C,
it is possible to return a function pointer from a function, but not to return
an actual function.

Secondly, OCaml functions are typically defined in a curried style. The signature of 
a two-argument function is written as follows:

``` ocaml
val curried : int -> int -> int
```

but this really means

``` ocaml
val curried : int -> (int -> int)
```

and the arguments can be supplied one at a time to create a closure.  In
contrast, C functions receive their arguments all at once.  The equivalent C
function type is the following:

```c
int uncurried_C(int, int);
```

and the arguments must always be supplied together:

```c
uncurried_C(3, 4);
```

A C function that's written in curried style looks very different:

```c
/* A function that accepts an int, and returns a function pointer that
   accepts a second int and returns an int. */
typedef int (function_t)(int);
function_t *curried_C(int);

/* supply both arguments */
curried_C(3)(4);

/* supply one argument at a time */
function_t *f = curried_C(3); f(4);
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

Arrays in C are contiguous blocks of the same value.  Any of the basic types
defined earlier can be allocated as blocks via the `Array` module.

```ocaml
module Array : sig
  type 'a t = 'a array

  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
  val of_list : 'a typ -> 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val length : 'a t -> int
  val start : 'a t -> 'a ptr
  val from_ptr : 'a ptr -> int -> 'a t
  val make : 'a typ -> ?initial:'a -> int -> 'a t
end
```

The array functions are similar to the standard library `Array` module except
that they represent flat C arrays instead of OCaml ones.

The conversion between arrays and lists still requires copying the values, and
can be expensive for large data structures.  Notice that you can also convert
an array into a `ptr` pointer to the head of buffer, which can be useful if you
need to pass the pointer and size arguments separately to a C function.

Unions in C are named structures that can be mapped onto the same underlying
memory.  They are also fully supported in Ctypes, but we won't go into more
detail here.

<note>
<title>Pointer operators for dereferencing and arithmeic</title>

Ctypes defines a number of operators that let you manipulate pointers and arrays
just as you would in C.  The Ctypes equivalents do have the benefit of being
more strongly typed, of course.

Operator    Purpose      
--------    -------
`!@ p`      Dereference the pointer `p`.
`p <-@ v`   Write the value `v` to the address `p`.
`p +@ n`    If `p` points to an array element, then compute the address of the `n`th next element.
`p -@ n`    If `p` points to an array element, then compute the address of the `n`th previous element.

</note>

## Passing functions to C

It's also straightforward to pass OCaml function values to C.  The C standard
library function `qsort` has the following signature that requires a function
pointer to use.

```c
void qsort(void *base, size_t nmemb, size_t size,
           int(*compar)(const void *, const void *));
```

C programmers often use `typedef` to make type definitions involving function
pointers easier to read.  Using a typedef, the type of `qsort` looks a little
more palatable.

```c
typedef int(compare_t)(const void *, const void *);
void qsort(void *base, size_t nmemb, size_t size, compare_t *);
```

This also happens to be a close mapping to the corresponding Ctypes definition.
Since type descriptions are regular values, we can just use `let` in place of
`typedef` and end up with working OCaml bindings to `qsort`.

```ocaml
let compare_t = ptr void @-> ptr void @-> returning int

let qsort = foreign "qsort"
   (ptr void @-> size_t @-> size_t @-> funptr compare_t @-> returning void)
```

We only use `compare_t` once (in the `qsort` definition), so you can choose to
inline it in the OCaml code if you prefer. The resulting `qsort` value is a
higher-order function, as shown by its type.

```ocaml
val qsort: void ptr -> size_t -> size_t ->
           (void ptr -> void ptr -> int) -> unit
```

As before, let's define a wrapper function to make `qsort` easier to use.  The
second and third arguments to `qsort` specify the length (number of elements)
of the array and the element size.

Arrays created using Ctypes have a richer runtime structure than C arrays, so
we don't need to pass size information around.  Furthermore, we can use OCaml
polymorphism in place of the unsafe `void ptr` type.

### Example: a command-line quicksort

Below is a command-line tool that uses the `qsort` binding to sort all of
the integers supplied on the standard input.

```
(* qsort.ml: quicksort integers from stdin *)
open Core.Std
open Ctypes
open PosixTypes
open Foreign

let compare_t = ptr void @-> ptr void @-> returning int

let qsort = foreign "qsort"
   (ptr void @-> size_t @-> size_t @-> funptr compare_t @-> 
    returning void)

let qsort' cmp arr =
  let open Unsigned.Size_t in
  let ty = Array.element_type arr in
  let len = of_int (Array.length arr) in
  let elsize = of_int (sizeof ty) in
  let start = to_voidp (Array.start arr) in
  let compare l r = cmp (!@ (from_voidp ty l)) (!@ (from_voidp ty r)) in
  qsort start len elsize compare;
  arr

let sort_stdin () =
  In_channel.input_lines stdin
  |> List.map ~f:int_of_string
  |> Array.of_list int
  |> qsort' Int.compare
  |> Array.to_list
  |> List.iter ~f:(fun a -> printf "%d\n" a)

let () =
  Command.basic ~summary:"Sort integers on standard input"
    Command.Spec.empty sort_stdin
  |> Command.run
```

Compile it in the usual way with ocamlfind, but also examine the inferred interface
of the module.

```console
$ ocamlfind ocamlopt -package core -package ctypes.foreign -thread -linkpkg \
    -o qsort qsort.ml
$ ./qsort
5
3
2
1
# press <Control-D> to end the standard input
1
2
3
5
$ ocamlfind ocamlopt -i -package core -package ctypes.foreign -thread qsort.ml
val compare_t : (unit Ctypes.ptr -> unit Ctypes.ptr -> int) Ctypes.fn
val qsort :
  unit Ctypes.ptr ->
  PosixTypes.size_t ->
  PosixTypes.size_t -> (unit Ctypes.ptr -> unit Ctypes.ptr -> int) -> unit
val qsort' : ('a -> 'a -> int) -> 'a Ctypes.array -> 'a Ctypes.array
val sort_stdin : unit -> unit
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

Values allocated via Ctypes (i.e. using `allocate`, `Array.make` and so on)
will not be garbage-collected as long as they are reachable from OCaml values.
The system memory they occupy is freed when they do become unreachable, via a
finalizer function registered with the GC.

The definition of reachability for Ctypes values is a little different from
conventional OCaml values though.  The allocation functions return an
OCaml-managed pointer to the value, and as long as some derivative pointer is
still reachable by the GC, the value won't be collected.

"Derivative" means a pointer that's computed from the original pointer via
arithmetic, so a reachable reference to an array element or a structure field
protects the whole object from collection.

A corollary of the above rule is that pointers written into the C heap don't
have any effect on reachability.  For example, if you have a C-managed array of
pointers to structs then you'll need some additional way of keeping the structs
around to protect them from collection.  You could achieve this via a global array
of values on the OCaml side that would keep them live until they're no longer
needed.

</note>

## Learning more about C bindings

The Ctypes [distribution](http://github.com/ocamllabs/ocaml-types) contains a
number of larger-scale examples, including:

* bindings to the POSIX `fts` API which demonstrates C callbacks more comprehensively.
* a more complete Ncurses binding than the example we opened the chapter with.
* a comprehensive test suite that covers the complete library, and can provide useful snippets for your own bindings.

This chapter hasn't really needed you to understand the innards of OCaml at
all.  Ctypes does its best to make function bindings easy, but the rest of this
part will also fill you in about how interactions with OCaml memory layout and
the garbage collector work.

<note>
<title>Production note</title>

This chapter contains significant contributions from Jeremy Yallop.

</note>
