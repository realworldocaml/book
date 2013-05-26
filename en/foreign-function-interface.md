# Foreign Function Interface

OCaml has several options available to interact with non-OCaml code.  The
compiler toolchain can link to external system libraries and also produce
standalone native object code that can be embedded within other non-OCaml
libraries or applications.  

## The `ctypes` foreign function library

The simplest foreign function interface in OCaml doesn't even require you to
write any C code.  The `ctypes` library lets you describe the C interface in
pure OCaml, and the library takes care of finding the C symbols and invoking
the function call with the appropriate arguments.

Let's dive straight into an example to show you how the library looks.  We'll
use a binding to the `ncurses` terminal toolkit, as it's widely available on
most systems and doesn't have any complex dependencies.

<note>
<title>Installing the `ctypes` library</title>

TODO `ctypes` is not yet available on OPAM, but will be soon.  For now, install
it manually:

```
$ git clone git://github.com/ocamllabs/ocaml-ctypes
$ cd ocaml-ctypes
$ make && make install
```

It will then be available via the `ctypes` ocamlfind package.  You will also
need the `ncurses` library for the first example.  It is pre-installed on MacOS
X and Debian Linux includes it as the `ncurses-dev` package.

</note>

## Example: an ncurses terminal interface

Ncurses is a library to build terminal-independent text interfaces in a
reasonably efficient way.  It's used in console mail clients like `mutt` and
`pine`, and console web browsers such as `lynx`.  The
[documentation](http://www.gnu.org/software/ncurses/) explains the full C
interface, but here's an excerpt that we need for a basic binding to OCaml.
The full header file can usually be found in `/usr/include/ncurses.h` on MacOS
X or Linux.

```c
typedef struct _win_st WINDOW;

WINDOW *initscr   (void);
WINDOW *newwin    (int, int, int, int);
void    endwin    (void);
void    refresh   (void);
void    wrefresh  (WINDOW *);
void    mvwaddstr (WINDOW *, int, int, char *);
```

The `ncurses` library calls either work on the current pseudo-terminal, or on a
window that has been created via the library.  The `WINDOW` typdef represents
this external `ncurses` library state.  The specific contents of the structure
don't matter; OCaml code just needs to store the pointer and pass it back to
`ncurses` library calls that then dereference its contents.

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
open Ctypes.Ffi.C
open Type

type window = unit ptr
let window = ptr void
```

We first define a `window` type to represent the C `WINDOW` pointer.  The
`unit ptr` type is equivalent to a `void *` pointer (we'll constrain the
signature later on to avoid mixing up different pointer types).  We also need a
value representing this `window` type to pass to the `ctypes` library, The
`window` value is built using the `Ctypes.Ffi.C.ptr` function.  The next step
is to use this value to build a foreign function call to `initscr`.

```ocaml
(* ncurses.ml 2/3 *)
let initscr =
  foreign "initscr" (void @-> (returning window))
```

The `foreign` function takes two parameters: the C function call name, and a
value that defines the C function arguments and return type.  This definition
can contain any of the C types (including function pointers), and is built
using functions defined in `Ctypes.Ffi.C`. 
 
Basic C types such as `void` are defined as values in `Ffi.Types.C`, and we
have previously defined `window` as a `void ptr`.  The `@->` operator adds an
argument to the C parameter list and the `returning` function terminates the
parameter list and declares the return value.  The remainder of the `Ncurses`
implementation expands on these definitions for the other library functions.

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
earlier in the chapter.  They use the basic C types defined in `FFi.Types.C`
such as `void` or `int`.  The `string` value maps from OCaml strings (which
have a specific length) onto C character buffers (whose length is defined by a
null characters).

The module signature for `ncurses.mli` looks much like a normal OCaml
signature. You can infer it from `ncurses.ml` by running:

```console
$ ocamlfind ocamlc -i -package ctypes ncurses.ml
```

We've tweaked the automatic signature to make the `type window` abstract,
and the result is below:

```ocaml
type window

val window    : window Ffi.C.Type.t
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

The `window` type is left abstract to external users so that it can only be
constructed via the `Ncurses.initscr` function.  This interface is now safe to
use externally, since window pointers cannot be mixed up with other `void`
pointers (e.g. those obtained by other libraries).  Here's what a "hello world"
that uses the library looks like:

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

This code can be compiled by linking against the `ctypes` and `unix` ocamlfind packages.

```console
$ ocamlfind ocamlopt -linkpkg -package ctypes -package unix \
  -cclib -lncurses ncurses.mli ncurses.ml hello.ml -o hello
```

Running `./hello` should now display a Hello World in your terminal with a box
around the "World"!  The compilation line  includes `-cclib -lncurses` to tell
the OCaml compiler to link the output binary to the `ncurses` C library, which
in turns makes the symbols available to the program when it starts.  If you
omit that line, you'll get an error when you try to run the binary:

```console
$ ocamlfind ocamlopt -linkpkg -package ctypes -package unix \
  ncurses.mli ncurses.ml hello.ml -o hello_broken
$ ./hello_broken 
Fatal error: exception Dl.DL_error("dlsym(RTLD_DEFAULT, initscr): symbol not found")
```

## Defining basic C formats from OCaml

The `Ctypes` library provides an `FFi.C` module that lets you describe not only
basic C types, but also more complex structures and unions.  It defines
abstract OCaml types for all of these in `FFi.C`:

```ocaml
type 'a typ        (** Basic C type  *)
type 'a ptr        (** C pointer *)
type 'a array      (** C array of 'a values *)
type 'a structure  (** C `struct` *)
type 'a union      (** C `union` *)
type 'a abstract   (** Abstract C pointer *)
```

The `Ffi.C.Type` module defines constructors for the familiar C basic types and
lets you build `'a typ` values.

```ocaml
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

These functions all return an `'a typ`, where the `'a` component is the OCaml
representation of the C type. For example, OCaml only supports double-precision
floating point numbers, and so both of the C `float` and `double` functions map
to the OCaml `float` type.  The `Ffi.Unsigned` and `Ffi.Signed` modules provide
some optimized implementations of specific C types, such as `llong` (for `long
long` 64-bit values).

The module also defines some more advanced C types

```ocaml   
... 
  val string : string t
  val abstract : size:int -> alignment:int -> 'a abstract t
  val array : int -> 'a t -> 'a array t
  val ptr : 'a t -> 'a ptr t
```

Strings in C are null-terminator character arrays, whereas OCaml strings have a
fixed-length and can contain null values. The `string` mapping safely copies
between these two strings.  if you need an abstract C type (for example, from a
forward declaration of a `struct`), then just define an `abstract t`.  Arrays
and pointers can be built out of primitive types by using the corresponding
constructor functions.

```ocaml
...
  val ( @-> ) : 'a t -> 'b f -> ('a -> 'b) f
  val returning : 'a t -> 'a f
  val funptr : ('a -> 'b) f -> ('a -> 'b) t
```

Sequences of `'a typ` values are constructed by using the `@->` and `returning`
functions.  You can even exchange function pointers between OCaml and C by
wrapping the OCaml callback using `funptr`.  The library takes care of the
garbage collector interface to ensure that the OCaml value isn't moved around
while the C library is holding a reference to the value.

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

The array functions are similar to the standard library `Array` module.  The
conversion between arrays and lists involves reallocating the values, and can
be expensive for very large data structures.  Notice that you can also convert
an array into a `ptr` pointer to the head of buffer, which can be useful if you
need to pass the pointer and size arguments separately to a C function.

Structures in C can contain a mixture of types, and, like OCaml records, their
order is significant.  The `Ffi.C.Type.Struct` module defines combinators to make
this as easy as arrays and basic types.  Let's start with an example by binding
some time-related UNIX functions.

### Example: binding UNIX date functions

The UNIX standard C library defines several useful time and date functions 
in `<time.h>` (usually found in `/usr/include` on a Linux or MacOS X system).
The `localtime` function has the following signature and return value:

```c
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
to allocate some memory to store a `time_t` value, and pass that memory into
the `time` library call to obtain the current timezone.  This `time_t` is then
passed to the `localtime` library call, which returns a pointer to the `struct
tm`. 

The `time_t`, and most other standard POSIX types, are already provided by the `Ffi.PosixTypes` module.
Let's start by defining the OCaml mapping to `struct tm`:

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

This is a very mechanical translation from the C structure, due to the magic of
the `*:*` combinator provided by the `Struct` module.  We start building the
definition via the `structure` allocator.  Then the types are added in
sequence, with each application recording the position in the `tm` structure.
When all the fields have been added, the structure is finalized via the `seals`
call.  The definitions of `time` and `localtime` should now be familiar:

```ocaml
let time = foreign "time" (ptr time_t @-> syscall time_t)
let asctime = foreign "asctime" (ptr tm @-> returning string)
let localtime = foreign "localtime" (ptr time_t @-> returning (ptr tm))
```

The complete signature for this definition looks like this:

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

Unlike the ncurses example, some of the FFI types are still exposed in this
signature due to the manual memory interface required by the C libraries.  To
use the OCaml `time` and `localtime` functions, we need to allocate some memory
and construct values of type `time_t ptr` to them.

```ocaml
let () =
  let timep = Ptr.allocate ~count:1 time_t in
  let time = time timep in
  let tm = localtime timep in
  print_endline (asctime tm)
```

The `Ptr.allocate` function allocates memory via `malloc` and creates an OCaml
value to point to it.  This OCaml value (`timep` in the example) has a finalizer
function which frees the external memory when it is garbage collected.
The `timep` pointer is passed into the `time` library call, which modifies it
in-place.  The `timep` pointer is then passed on to `localtime`, whose return value
is converted into an OCaml string via `asctime`.   The garbage
collector can then collect `timep` during the next collection cycle.

Unions in C are a collection of named structures that can be mapped onto the
same memory.  They are also supported in the `ctypes` library via the
`Ffi.C.Union` module, although we won't go into more detail here.

## Callbacks between C and OCaml

TODO: the fts(3) interface.
