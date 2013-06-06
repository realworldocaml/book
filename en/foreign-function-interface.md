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
C types. You can also build up more complex C structures and unions in OCaml as
well.

```ocaml
(* Ctypes *)
type 'a typ        (** Basic C type  *)
type 'a ptr        (** C pointer *)
type 'a array      (** C array of 'a values *)
type 'a structure  (** C `struct` *)
type 'a union      (** C `union` *)
type 'a abstract   (** Abstract C pointer *)
```

The module also defines constructors for the familiar C basic types.
These constructors build a value of `Ctype.FFi.C.typ` that represents
that basic C type.

```ocaml
(* Ctypes.Ffi.C 1/3 *)
module Type : sig
  type 'a t = 'a typ 
  
  val void  : unit t
  val char : char t
  val schar : int t
  val float : float t
  val double : float t
  val short : int t
  val int   : int t
  val long  : long t
  val llong  : llong t
  val nativeint : nativeint t
  val int8_t : int t
  val int16_t : int t
  val int32_t : int32 t
  val int64_t : int64 t
...
```

These functions return an `'a typ` where the `'a` component is the OCaml
representation of the C type. For example, OCaml only supports double-precision
floating point numbers and so the C `float` and `double` functions both map to
the OCaml `float` type.

The `Ffi.Unsigned` and `Ffi.Signed` modules provide optimized implementations
of C types such as `llong` (for `long long` 64-bit values) or `int32_t` (for
signed 32-bit values).
The module also defines some more advanced C types that aren't straightforward
mappings to and from OCaml.

```ocaml   
(* Ctypes.Ffi.C 2/3 *)
  val string : string t
  val abstract : size:int -> alignment:int -> 'a abstract t
  val array : int -> 'a t -> 'a array t
  val ptr : 'a t -> 'a ptr t
```

Strings in C are null-terminated character arrays, while OCaml strings have a
fixed-length specified in the value header. The `string` function creates a
safe mapping between these two representations by copying the data to and from
OCaml strings and C character buffers.

Arrays and pointers can be built from basic types by using the corresponding
`array` and `ptr` functions.  The `abstract` function accepts size and
alignment requirements and ensures that these are satisfied when this type is
used in a function call.  Notice that the result types of these functions all
share the same `Ffi.Type.C.t` type as the basic C type definitions, which means
that they can all be used interchangeably.

The next step is to group collections of C types into function definitions,
which are represented by type `'a Ffi.Type.f`.

```ocaml
(* Ctypes.Ffi.C 3/3 *)
  type 'a f
  val ( @-> ) : 'a t -> 'b f -> ('a -> 'b) f
  val returning : 'a t -> 'a f
  val funptr : ('a -> 'b) f -> ('a -> 'b) t
```

Sequences of `'a typ` values are constructed by using the `@->` and `returning`
functions.  You can even exchange function pointers between OCaml and C by
wrapping the OCaml callback using `funptr`.  The library takes care of the
garbage collector interface to ensure that the OCaml value isn't moved around
while the C library is holding a reference to the value.  We'll come back to an
example of using `funptr` later in the chapter.

### Arrays, structures and unions

Arrays in C are contiguous blocks of the same value.  Any of the basic types
defined earlier can be allocated as blocks via the `Ffi.C.Type.Array` module.

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
order is significant.  The `Ffi.C.Type.Struct` module defines combinators to
make this definition as easy basic types were.  Let's look at an with an
example by binding some time-related UNIX functions that use C structures
in their interface.

### Example: binding UNIX date functions

The UNIX standard C library defines several useful time and date functions 
in `<time.h>` (usually found in `/usr/include` on a Linux or MacOS X system).
The `localtime` function has the following signature and return value:

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
open Ffi.C
open Type
open PosixTypes
open Struct

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
let () = seals (tm : tm structure typ)
```

This is a fairly mechanical translation from the C structure by using the
magic of the `*:*` combinator provided by the `Ffi.C.Struct` module.  The
structure is initialised in the `tm` variable via the `structure` allocator.
The fields of the structure are then added in sequence. Each new field mutates
the `tm` structure to append its name and offset.  The structure is finalized
via `seals` when all the fields have been added, and the structure can now
be used.

The OCaml definitions of `time` and `localtime` are now straightforward calls
to `foreign`, just like our earlier `ncurses` example.

```ocaml
let time = foreign "time" (ptr time_t @-> syscall time_t)
let asctime = foreign "asctime" (ptr tm @-> returning string)
let localtime = foreign "localtime" (ptr time_t @-> returning (ptr tm))
```

The OCaml signature for this definition looks like this:

```ocaml
open Ffi.C
type tm
val tm : tm structure typ
val tm_sec : (int, tm) Struct.field
val tm_min : (int, tm) Struct.field
val tm_hour : (int, tm) Struct.field
val tm_mday : (int, tm) Struct.field
val tm_mon : (int, tm) Struct.field
val tm_year : (int, tm) Struct.field
val tm_wday : (int, tm) Struct.field
val tm_yday : (int, tm) Struct.field
val tm_isdst : (int, tm) Struct.field

val time : PosixTypes.time_t ptr -> PosixTypes.time_t
val localtime : PosixTypes.time_t ptr -> tm structure ptr
val asctime : PosixTypes.time_t ptr -> string
```

Some of the FFI types are still exposed in this signature due to the manual
memory interface required by the C libraries.  The OCaml `time` and `localtime`
can be used by allocating external memory and constructing values of type
`time_t ptr`.

```ocaml
let () =
  let timep = Ptr.allocate ~count:1 time_t in
  let time = time timep in
  let tm = localtime timep in
  print_endline (asctime tm)
```

The `Ptr.allocate` function allocates memory via `malloc` and creates an OCaml
value to point to this external memory buffer.  This OCaml value (`timep` in the
example) has a finalizer function which frees the external memory when it is
garbage collected.  The `timep` pointer is passed into the `time` library call,
which modifies it in-place.  The same pointer is subsequently passed to
`localtime`, whose return `tm` structure is converted into an OCaml string via
the `asctime` function.   The garbage collector is free to free `timep` during
the next collection cycle.

Unions in C are a collection of named structures that can be mapped onto the
same memory.  They are also supported in the `ctypes` library via the
`Ffi.C.Union` module, although we won't go into more detail here.

## Callbacks between C and OCaml

TODO: the fts(3) interface.
