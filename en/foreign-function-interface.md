# Foreign Function Interface

OCaml has several options available to interact with non-OCaml code.  The
compiler can link to external system libraries via C code, and also produce
standalone native object files that can be embedded within other non-OCaml
applications. 

## The Ctypes foreign function library

The simplest foreign function interface in OCaml doesn't even require you to
write any C code.  The `ctypes` library lets you define the C interface in pure
OCaml, and the library takes care of dynamically loading the C symbols and
invoking the function call with the appropriate arguments.

Let's dive straight into an example to show you how the library looks.  We'll
use a binding to the `ncurses` terminal toolkit, as it's widely available on
most systems and doesn't have any complex dependencies.

<note>
<title>Installing the Ctypes library</title>

Ctypes is available via OPAM as usual.  You'll need to install the
[`libffi`](https://github.com/atgreen/libffi) library before starting the OPAM
installtion . It's a fairly popular library and should be available in your OS
package manager.

A special note for Mac user: the version of `libffi` installed by default in
MacOS X 10.8 is too old for some of the features that Ctypes needs.  Use
Homebrew to `brew install libffi` to get the latest version before installing
the OCaml library.

```
$ brew install libffi     # for MacOS X users
$ opam install ctypes
```

It will then be available via the `ctypes` OCamlfind package.  You'll also need
the `ncurses` library for the first example.  This comes pre-installed on MacOS
X and Debian Linux provides it as the `ncurses-dev` package.

</note>

## Example: an ncurses terminal interface

Ncurses is a library to help build terminal-independent text interfaces in a
reasonably efficient way.  It's used in console mail clients like `mutt` and
`pine`, and console web browsers such as `lynx`.

The full C interface is quite large and explained in the online
[documentation](http://www.gnu.org/software/ncurses/).  We'll use the excerpt
below since it's all we need for a basic binding in OCaml.  The header
file is usually installed in `/usr/include/ncurses.h`.

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
window that has been created via the library.  The `WINDOW` typdef represents
the library state and is considered abstract to users of the library.  OCaml
code just needs to store the pointer somewhere and pass it back to Ncurses.
library calls that then dereference its contents.

There are two library calls that create `WINDOW` pointers. The `initscr`
function initialises the library and returns the global window, and `newwin`
allows further windows to be created.  The `WINDOW` pointer can also be passed
to terminal drawing functions such as `mvwaddrstr` (there are over 200 library
calls in `ncurses`, so we are just binding a select few for this example).  The
terminal is updated when `refresh` or `wrefresh` are called. All other drawing
calls just manipulate library data structures without actually changing the
screen layout.

The `ctypes` library provides an OCaml interface that lets you declare these C
functions as OCaml values.  The library takes care of converting the OCaml
arguments into the C calling convention, invoking the foreign call within the
`ncurses` library, and finally returning the result as an OCaml value.

```ocaml
(* ncurses.ml 1/3 *)
open Ctypes

type window = unit ptr
let window : window typ = ptr void
```

We first define a `window` type to represent the C `WINDOW` pointer.  The `unit
ptr` type represents a `void *` pointer in C, but we'll constrain the signature
later on to avoid mixing up different void pointers.

We also define a value representing a pointer to the `window` type, which can
be used later in the Ctypes function definitions.  The next step is to to build
a foreign function call to `initscr`.

```ocaml
(* ncurses.ml 2/3 *)
let initscr =
  foreign "initscr" (void @-> (returning window))
```

The `foreign` function is defined takes two parameters:

- the C function call name, which is looked up using the *dlsym(3)* linker function.
- a value that defines all the C function arguments and return type.
  Basic C types such as `void` are defined as values in `Ctypes` and we
  defined `window` in our own code.  The `@->` operator adds an
  argument to the C parameter list, while the `returning` function terminates the
  parameter list with the return type.

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

These definitions are all straightforward mappings from the C headers from
earlier in the chapter.  They use the basic C types defined in `Ctypes`
such as `void` or `int`.  The `string` value maps from OCaml strings (which
have a specific length) onto C character buffers (whose length is defined by a
null characters).

The module signature for `ncurses.mli` looks much like a normal OCaml
signature. You can infer it from `ncurses.ml` by running:

```console
$ ocamlfind ocamlc -i -package ctypes.foreign ncurses.mli 
```

We've tweaked the automatic signature to make the `type window` abstract,
and the result is below:

```ocaml
type window

val window    : window Ctypes.typ
val initscr   : unit   -> window
val endwin    : unit   -> unit
val refresh   : unit   -> unit
val wrefresh  : window -> unit
val newwin    : int    -> int -> int -> int -> window
val addch     : char   -> unit
val mvwaddch  : window -> int -> int -> char -> unit
val addstr    : string -> unit
val mvwaddstr : window -> int -> int -> string -> unit
val box       : window -> int -> int -> unit
val cbreak    : unit   -> unit
```

The `window` type is left abstract in the signature so that it can only be
constructed via the `Ncurses.initscr` function.  This makes the Ncurses binding
safer to use externally, since window pointers cannot be mixed up with other
`void` pointers obtained by other libraries.

Here's what a "hello world" that uses the library looks like:

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

This code can be compiled by linking against the `ctypes` and `ctypes.foreign`
OCamlfind packages.

```console
$ ocamlfind ocamlopt -linkpkg -package ctypes.foreign -cclib -lncurses \
    ncurses.mli ncurses.ml hello.ml -o hello
```

Running `./hello` should now display a Hello World in your terminal!

The command-line above includes `-cclib -lncurses` to make the OCaml compiler
link the output to the `ncurses` C library, which in turns makes the C symbols
available to the program when it starts.  You should get an error when you run
the binary if you omit that link directive.

```console
$ ocamlfind ocamlopt -linkpkg -package ctypes -package unix \
  ncurses.mli ncurses.ml hello.ml -o hello_broken
$ ./hello_broken 
Fatal error: exception Dl.DL_error("dlsym(RTLD_DEFAULT, initscr): symbol not found")
```

## Defining basic C formats from OCaml

`Ctypes` wouldn't be very interesting if it were limited to only defining basic
C types. You can build up more complex C structures and unions using it as
well.

### Defining basic scalar types

Let's go over over some of the basic Ctypes definitions first.

```ocaml
(* Ctypes 1/4 *)
type 'a typ
```

This is the type of values representing C types.  There are two types associated
with each `typ` value: the C type used to store and pass values and the
corresponding OCaml type.

The `'a` type parameter indicates the OCaml type, so a value of type `t typ` is
used to read and write OCaml values of type `t`.  There are various uses of
`typ` values within Ctypes.

* constructing function types for binding native functions using the `Foreign` module, as shown earlier.
* constructing pointers for reading and writing locations in C-managed storage using `ptr`.
* describing the fields of structured types built with `structure` and `union`.

The other core types follow the same style, with the type parameter defining
the corresponding OCaml value for that C type.

Type             Purpose
----             -------

`'a ptr`         The type of pointer values, used to read and write values of type '`a' at particular addresses.
`'a array`       C array
`'a structure`   C `struct` types
`'a union`       C `union` types
'`a abstract`    The type of abstract values, used represent values whose type varies from platform to platform.


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

Ctypes also defines constructors for the familiar C scalar types.

```ocaml
(* Ctypes.Ffi.C 2/4 *)
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

These return an `'a typ` where the `'a` component is the OCaml representation
of the C type. Some of these mappings are due to OCaml's in-memory
representation of values, which we explain later in
[xref](#memory-representation-of-values).

* Void values appear in OCaml as the `unit` type, so using `void` in an argument 
  or result type specification produces a function which accepts or returns unit.
  Dereferencing a pointer to `void` is an error, as in C, and will raise the `IncompleteType` exception.
* The C `size_t` type is an alias for one of the unsigned integer types.  The actual 
  size and alignment requirements for `size_t` varys between platforms. Ctypes provides
  an OCaml `size_t` type that is aliased to the appropriate integer type.
* OCaml only supports double-precision floating point numbers, so the C `float` and
  `double` functions both map to the OCaml `float` type.

### Declaring strings, arrays and pointers

Ctypes also contains the more advanced C types that build over the basic scalar C types defined earlier.

```ocaml   
(* Ctypes 3/4 *)
val string : string typ
val abstract : size:int -> alignment:int -> 'a abstract typ
val array : int -> 'a t -> 'a array typ
val ptr : 'a t -> 'a ptr typ
```

Strings in C are null-terminated character arrays, while OCaml strings have a
fixed-length specified in the value header. The `string` function creates a
safe mapping between these two representations by copying the data to and from
OCaml strings and C character buffers.  This avoids any problems with the
garbage collector relocating buffers that a C library expects to remain
immovable.

Arrays and pointers can be built from basic types by using the corresponding
`array` and `ptr` functions.  The `abstract` function accepts size and
alignment requirements and ensures that these are satisfied when this type is
used in a function call.

Notice that the result types of these functions all share the same `Ctypes.typ`
type that the scalar type definitionss.  This means that they can be used
interchangeably, for example to create an `float array typ` to represent a
buffer of floating-point numbers.

### Assembling function declarations

The next step is to arrange sets of C types into function definitions that
match the C function calls.  A value of type `'a fn` is used to bind to C
functions and to describe the type of OCaml functions passed to C.

```ocaml
(* Ctypes.Ffi.C 4/4 *)
type 'a fn
val ( @-> ) : 'a typ -> 'b fn -> ('a -> 'b) fn
val returning : 'a typ -> 'a fn
val funptr : ?name:string -> ('a -> 'b) fn -> ('a -> 'b) typ
```

A function is declared by composing sequences of `'a typ` values using the
`@->` operator, and closing it with `returning` to define the return type.

The ctypes library, like C itself, distinguishes *functions* and *function
pointers*. Functions are not first class: it isn't possible to use them as
arguments or return values of calls, or store them in addressable memory.
Function pointers are first class, and so have none of these restrictions.
Function pointers are defined using `funptr` and can be passed around as any
other `typ` value in argument lists.

### Arrays, structures and unions

Arrays in C are contiguous blocks of the same value.  Any of the basic types
defined earlier can be allocated as blocks via the `Ctypes.Array` module.

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

The array functions are similar to the standard library `Array` module, except
that they represent flat C arrays instead of OCaml ones.  The conversion
between arrays and lists still requires copying the values, and can be
expensive for large data structures.  Notice that you can also convert an array
into a `ptr` pointer to the head of buffer, which can be useful if you need to
pass the pointer and size arguments separately to a C function.

Structures in C can contain a mixture of types, and, like OCaml records, their
order is significant.  The `Ctypes.Struct` module defines combinators to
make this definition as easy as using the scalar types.

Let's look at an example by binding some time-related UNIX functions that use C
structures in their interface.

#### Example: binding UNIX date functions

The UNIX standard C library defines several useful time and date functions in
`<time.h>` (usually found in `/usr/include` on a Linux or MacOS X system).  The
`localtime` function has the following signature and return value:

```c
/* /usr/include/time.h */

struct tm {
  int     tm_sec;         /* seconds after the minute [0-60] */
  int     tm_min;         /* minutes after the hour [0-59] */
  int     tm_hour;        /* hours since midnight [0-23] */
  int     tm_mday;        /* day of the month [1-31] */
  int     tm_mon;         /* months since January [0-11] */
  int     tm_year;        /* years since 1900 */
  int     tm_wday;        /* days since Sunday [0-6] */
  int     tm_yday;        /* days since January 1 [0-365] */
  int     tm_isdst;       /* Daylight Savings Time flag */
};

time_t time(time_t *);
struct tm *localtime(const time_t *);
```

This example is more complicated than ncurses for a couple of reasons.  We need
to allocate some external memory to store a `time_t` value, and pass that
memory into the `time` library call to obtain the current timezone.  This
`time_t` value is passed to the `localtime` library call, which then returns a
pointer to the `struct tm`. 

The `time_t` and many other standard POSIX types are already provided by the
`Ffi.PosixTypes` module.  Let's start by defining the OCaml mapping to
`struct tm`:

```ocaml
(* ffi_date.ml *)
open Ctypes
open PosixTypes

type tm
let tm = structure "tm"
let tm_sec   = tm *:* int (* seconds *)
let tm_min   = tm *:* int (* minutes *)
let tm_hour  = tm *:* int (* hours *)
let tm_mday  = tm *:* int (* day of the month *)
let tm_mon   = tm *:* int (* month *)
let tm_year  = tm *:* int (* year *)
let tm_wday  = tm *:* int (* day of the week *)
let tm_yday  = tm *:* int (* day in the year *)
let tm_isdst = tm *:* int (* daylight saving time *)
let () = seal (tm : tm structure typ)
```

This code looks like a fairly mechanical translation from the C structure
definition thanks to the magic of the Ctypes `*:*` combinator.

The `tm` structure is initialised via the `structure` allocator and its fields
are added sequentially. Every invocation of `*:*` mutates `tm` to record its
type and offset within the structure.  The structure is finalized via `seal`
after all the fields have been added.  A sealed structure has a concrete size
and alignment and can now be used in other type definition.

The OCaml definitions of `time` and `localtime` are now straightforward calls
to `foreign`, just like our earlier `ncurses` example.

```ocaml
open Foreign

let time =
  foreign "time" (ptr time_t @-> returning_checking_errno time_t)

let asctime =
  foreign "asctime" (ptr tm @-> returning string)

let localtime =
  foreign "localtime" (ptr time_t @-> returning (ptr tm))
```

The OCaml signature for this definition looks like this:

```ocaml
(* ffi_date.mli *)

open Ctypes
open PosixTypes

type tm
val tm_sec : (int, tm structure) field
val tm_min : (int, tm structure) field
val tm_hour : (int, tm structure) field
val tm_mday : (int, tm structure) field
val tm_mon : (int, tm structure) field
val tm_year : (int, tm structure) field
val tm_wday : (int, tm structure) field
val tm_yday : (int, tm structure) field
val tm_isdst : (int, tm structure) field

val time : time_t ptr -> time_t
val asctime : tm structure ptr -> string
val localtime : time_t ptr -> tm structure ptr
```

The structure fields are exposed as separate values to provide a way to 
extract their values from a buffer at runtime.  Let's see how to tie
these functions together.

```ocaml
let () = begin
  let timep = allocate_n ~count:1 time_t in
  let time = time timep in
  assert (time = !@timep);
  let tm = localtime timep in
  Printf.printf "tm.tm_mon  = %d\n" (getf !@tm tm_mon);
  Printf.printf "tm.tm_year = %d\n" (getf !@tm tm_year);
  print_endline (asctime tm)
end
```

The `allocate_n` is analagous to a type-safe version of the POSIX `calloc`
function.  The freshly allocated `timep` buffer is automatically garbage
collected as usual when it's no longer referenced within the OCaml code.

The `timep` pointer is passed into the `time` library call which modifies it
in-place (and, due to a quirky historical interface, also returns a pointer to
the same buffer).  This duplicated return value gives us an excuse to try out
the `!@` operator, which dereferences the structure pointer and provides access
to the structure value.

The same `timep` pointer is subsequently passed to `localtime`, which returns a
pointer to a `tm` structure.  We dereference a couple of its fields and print
them from OCaml, and then pass it to `asctime` to print a nice human-readable
time.


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

Unions in C are named structures that can be mapped onto the same underlying
memory.  They are also fully supported in in Ctypes, but we won't go into more
detail here.

## Callbacks between C and OCaml

TODO: the fts(3) interface.
