# Foreign Function Interface

OCaml has several options available to interact with non-OCaml code.  The
compiler toolchain can of course link to external system libraries, but can
also produce shared libraries that let your OCaml code be called from C, or
even encode your OCaml as a standalone C file that be compiled directly into
other libraries.

In this chapter, we'll first describe the easiest way to bind to C code, via
the `ctypes` library.  This can be done without writing a single line of C
code, and just requires a pure OCaml definition of the C interfaces.  We'll use
the `ncurses` terminal-drawing toolkit as our running example, as it's widely
available on most systems.

<note>
<title>Installing the `ctypes` library</title>

TODO `ctypes` is not yet available on OPAM, but will be soon.  For now, install
it manually:

```
$ git clone git://github.com/ocamllabs/ocaml-ctypes
$ cd ocaml-ctypes
$ make && make install
```

It will then be available via the `ctypes` ocamlfind package.

</note>

## Example: an ncurses terminal interface

Ncurses is a library to build terminal-independent text interfaces in a
reasonably efficient way.  The  manual page (usually via `man ncurses`)
explains the basics of the C interface.  The `initscr` function allocates some
state and returns it as a point to the C `WINDOW` typedef.  This C pointer is
then passed to various terminal drawing functions.

The `ctypes` library provides a combinator interface that lets you declare
these C functions as OCaml values.  The library takes care of converting the
OCaml arguments into the C calling convention, invoking the foreign call, and
returning the result as an OCaml value.

```ocaml
open Ctypes.Ffi.C
open Type

type window = unit ptr
let window = ptr void
```

The `window` type represents the `WINDOW *` pointer that ncurses returns.  This
can be left abstract in the OCaml signature for this module, and is represented
as a `ptr void` ("pointer to void") value within the implementation.

```ocaml
let initscr =
  foreign "initscr" (void @-> (returning window))
```

The `foreign` function takes two parameters: the C function call name, and a
definition of the arguments and return type.  The definition is built up using
the combinators defined in `Ctypes.Ffi.C`.  The `@->` operator adds an argument
to the parameter list, and the  `returning` function declares the return value.
The remainder of the `Ncurses` implementation expands on these definitions for
the other library functions.

```ocaml
let endwin =
  foreign "endwin" (void @-> (returning void))

let refresh =
  foreign "refresh" (void @-> (returning void))

let wrefresh =
  foreign "wrefresh" (window @-> (returning void))

let newwin =
  foreign "newwin" (int @-> int @-> int @-> int @-> (returning window))

let mvwaddch =
  foreign "mvwaddch" (window @-> int @-> int @-> char @-> (returning void))

let addstr =
  foreign "addstr" (string @-> (returning void))

let mvwaddstr =
  foreign "mvwaddstr" (window @-> int @-> int @-> string @-> (returning void))

let box =
  foreign "box" (window @-> int @-> int @-> (returning void))

let cbreak =
  foreign "cbreak" (void @-> (returning void))
```

These function calls all have different arguments, but the basic C types are
provided by the `Ffi.C` module and can be directly used.  The module signature
for `ncurses.mli` looks much like a normal OCaml signature:

```ocaml
type window
val window : window Ffi.C.Type.t
val initscr : unit -> window
val endwin : unit -> unit
val refresh : unit -> unit
val wrefresh : window -> unit
val newwin : int -> int -> int -> int -> window
val addch : char -> unit
val mvwaddch : window -> int -> int -> char -> unit
val addstr : string -> unit
val mvwaddstr : window -> int -> int -> string -> unit
val box : window -> int -> int -> unit
val cbreak : unit -> unit
```

Notice that the `window` type is left abstract to external users, so that it
can only be constructed via `Ncurses.initscr`.  This interface is now safe to use in
example code.  Here's what an ncurses hello world looks like:

```ocaml
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

This code can be compiled by:

```console
$ ocamlfind ocamlopt -linkpkg -package ctypes -package unix \
  -cclib -lncurses ncurses.mli ncurses.ml hello.ml -o hello
```

Running `./hello` should now display a Hello World in your terminal with a box
around the "World".  Notice that the compilation line  includes `-cclib
-lncurses`.  This tells the OCaml compiler to link the output binary to the
ncurses library, which in turns makes the symbols available to the program when
it starts.  If you omit that line, you'll get an error when you try to run the
binary:

```console
$ ocamlfind ocamlopt -linkpkg -package ctypes -package unix \
  ncurses.mli ncurses.ml hello.ml -o hello_broken
$ ./hello_broken 
Fatal error: exception Dl.DL_error("dlsym(RTLD_DEFAULT, initscr): symbol not found")
```

## Defining C values in OCaml

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
lets you build `'a typ' values.

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

These functions all return an `'a typ', where the `'a` component is the OCaml
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

#### Example: binding UNIX date functions

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

