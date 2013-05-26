# Compiler Output Formats

The process of converting OCaml source code to executable binaries is done in
multiple steps.  Every stage generally checks and discards information from the
source code, until the final output is untyped and low-level assembly code.

Each of the compilation steps can be executed manually if you need to inspect
something to hunt down a bug or performance regression.  It's even possible to
compile OCaml to run efficiently on environments such as Javascript or the Java
Virtual Machine.

In this chapter, you'll learn:

* The compilation pipeline and what each stage represents.
* TODO

## The compilation pipeline

The OCaml compiler initially accepts textual source code as input.  Each source
file is a separate *compilation unit* and can be compiled separately.  The
compilation pipeline looks like this:

```
    Source code
        |
        | parsing and preprocessing
        v
    Parsetree (untyped AST)
        |
        | syntax extensions
        v
    Camlp4 transformation (untyped AST)
        |
        | type inference and checking
        v
    Typedtree (type-annotated AST)
        |
        | pattern-matching compilation
        | elimination of modules and classes
        v
     Lambda
      /  \
     /    \ closure conversion, inlining, uncurrying,
    v      \  data representation strategy
 Bytecode   \
             \
            Cmm
             |
             | code generation
             v
        Assembly code
```

We'll now go through these stages and explain how the tools behind them operate.

### Parsing and preprocessing with `camlp4`

The first thing the compiler does is to parse the input source code into
a more structured data type.  This immediately eliminates code which doesn't
match basic syntactic requirements.  The OCaml lexer and parser use the same
basic techniques described earlier in [xref](parsing-with-ocamllex-and-menhir).

One powerful feature present in OCaml is the facility to dynamically extend the
syntax via the `camlp4` tool.  The compiler usually lexes the source code into
tokens, and then parses these into an Abstract Syntax Tree (AST) that represents the
parsed source code.

Camlp4 modules can extend the lexer with new keywords, and later transform these
keywords (or indeed, any portion of the input program) into conventional OCaml
code that can be understood by the rest of the compiler.  We've already seen
several examples of using `camlp4` within Core:

* **Fieldslib** to generates first-class values that represent fields of
  a record in [xref](#records).
* **Sexplib** to convert types to s-expressions in [xref](#data-serialization-with-s-expressions)
* **Bin_prot**: for efficient binary conversion in [xref](#fast-binary-serialization).

These all use a common `camlp4` library called `type_conv` to provide a common
extension point.  Type_conv defines a new keyword `with` that can appear after
a type definition, and passes on the type declaration to extensions.  The
type_conv extensions all generate boiler-plate code based on the type you
defined.  This approach avoids the inevitable performance hit of doing this
work dynamically, but also doesn't require a complex Just-In-Time (JIT) runtime
that is a source of unpredictable dynamic behaviour.

All `camlp4` modules accept an input AST and output a modified one.  This lets
you inspect the results of transformations at the source code level manually to
see exactly what's going on.  Let's look at a simple Core extension called
`pa_compare` for how to do this.

#### Example: the `pa_compare` syntax transformer

OCaml provides a polymorphic comparison operator that inspects the runtime
representation of two values to see if they are equal.  As we noted in
[xref](maps-and-hashtables], this is not as efficient or as safe as defining
explicit comparison functions between values.

The `pa_compare` syntax extension takes care of this boilerplate code
generation via `camlp4`. Try it out from `utop`:

```ocaml
# #require "comparelib.syntax" ;;

# type t = { foo: string; bar : t } ;;
type t = { foo : string; bar : t; }

# type t = { foo: string; bar: t } with compare ;;
type t = { foo : string; bar : t; }
val compare : t -> t -> int = <fun>
val compare_t : t -> t -> int = <fun>
```

The first type definition of `t` is a standard OCaml phrase and results in the
expected output.  The second one includes the `with compare` directive.  This
is intercepted by `comparelib` and turned into two new functions that are generated
from the type into the `compare` and `compare_t` functions.  How do we see what
these functions actually do?  You can't do this from `utop` directly, since it
embeds the `camlp4` compilation as an automated part of its operation.

Let's turn to the command-line to inspect the result of the `comparelib`
transformation instead.  Create a file that contains the type declaration from earlier:

```ocaml
(* comparelib_test.ml *)
type t = { foo: string; bar: t } with compare
```

Now create a shell script to run the `camlp4` tool manually.

```bash
#!/bin/sh
# camlp4_dump

OCAMLFIND="ocamlfind query -predicates syntax,preprocessor -r"
INCLUDE=`$OCAMLFIND -i-format comparelib.syntax`
ARCHIVES=`$OCAMLFIND -a-format comparelib.syntax`
camlp4o -printer o $INCLUDE $ARCHIVES $1
```

This shell script uses the `ocamlfind` package manager to list the include and
library paths required by the `comparelib` syntax extension.  The final
`camlp4o` command invokes the `camlp4o` preprocessor directly and outputs the
resulting AST to standard output as textual source code.

```console
$ sh camlp4_dump comparelib_test.ml
type t = { foo : string; bar : t }

let _ = fun (_ : t) -> ()
  
let rec compare : t -> t -> int =
  fun a__001_ b__002_ ->
    if Pervasives.( == ) a__001_ b__002_
    then 0
    else
      (let ret =
         (Pervasives.compare : string -> string -> int) a__001_.foo
           b__002_.foo
       in
         if Pervasives.( <> ) ret 0
         then ret
         else compare a__001_.bar b__002_.bar)
  
let _ = compare
let compare_t = compare
let _ = compare_t
```

The result is the original type definition, and some automatically generated
code that implements an explicit comparison function for each field in the
record.  This generated code is then compiled as if you had typed it in
yourself.

Another useful feature of `type_conv` is that it can generate signatures too.
Copy the earlier type definition into a `comparelib_test.mli` and rerun the
camlp4 dumper script.

```console
$ ./camlp4_dump.sh test_comparelib.mli 
type t = { foo : string; bar : t }

val compare : t -> t -> int
```

The external signature generated by `comparelib` is much simpler than the
actual code.  Running `camlp4` directly on the original source code lets you
see these all these transformations precisely.

<note>
<title>Don't overdo the syntax extensions</title>

Syntax extensions are a very powerful extension mechanism that can completely
change your source code's layout and style.  Core includes a very conservative
set of extensions that minimise the syntax changes.  There are a number of
third-party libraries that perform much more wide-sweeping changes, such as
introducing whitespace-sensitive indentation or even building entirely new
languages.

While it's tempting to compress all your boiler-plate code into `camlp4`
extensions, it can make production source code much harder for other people to
read and review.  Core mainly focuses on type-driven code generation using the
`type_conv` extension, and doesn't fundamentally change the OCaml syntax.

Another thing to consider before deploying your own syntax extension is
compatibility with other syntax extensions.  Two separate extensions create a
grammar clash can lead to hard-to-reproduce bugs. That's why most of Core's
syntax extensions go through `type_conv`, which acts as a single point for
extending the grammar via the `with` keyword.

</note>

### The type checking phase

### The lambda form

### Bytecode and `ocamlrun`

### Native code generation

## Interfacing with C

Now that you understand the runtime structure of the garbage collector, you can
interface it with C.  OCaml defines an `external` keyword that maps OCaml
functions to a C symbol.  When the function is invoked from OCaml, the C
function will be called with the OCaml function arguments using their native
`value` representation. This corresponds to the memory layout for OCaml values
described earlier.

### A "Hello World" C binding

Let's define a simple "Hello World" C binding to see how this works.
First create a `hello.ml` that contains the external declaration:

```ocaml
external hello_world: unit -> unit = "caml_hello_world"
let _ = hello_world ()
```

If you try to compile this module to an executable now, you should receive a
linker error:

```
$ ocamlopt -o hello hello.ml
Undefined symbols for architecture x86_64:
  "_caml_hello_world", referenced from:
      .L100 in hello.o
      _camlHello in hello.o
ld: symbol(s) not found for architecture x86_64
clang: error: linker command failed with exit code 1 (use -v to see invocation)
File "caml_startup", line 1:
Error: Error during linking
```

This is the system linker telling you that there is a missing
`caml_hello_world` symbol.  We need to provide a C file that will implement
this function and make it available to the linker before it creates a
standalone executable.  The OCaml compiler uses file extensions to determine
how to compile each file.  When it sees a `.c` extension, it passes it to the
system C compiler and appends an include directory containing the OCaml runtime
header files.  You can find these runtime header files by running `ocamlc
-where` and looking under the `caml/` subdirectory.

`mlvalues.h` is the basic header file that all OCaml-C bindings need.  It is
also shared by the garbage collector, and defines a few important typedefs early on
that should be familiar after the earlier explanation about the memory
representation of OCaml values:

```c
typedef intnat value;

#define Is_long(x)   (((x) & 1) != 0)
#define Is_block(x)  (((x) & 1) == 0)

#define Val_unit Val_int(0)
```

The `value` typedef is a memory word that can either be an integer if `Is_long` is
true, or a heap block if `Is_block` is true.  All of the arguments passed to the
C bindings will be of type `value`, since this is sufficient to represent
any valid OCaml value in memory.  Let's look at the external declaration for
`hello_world` again:

```ocaml
external hello_world: unit -> unit = "caml_hello_world"
```

This external function has a single argument of type `unit`, which is
represented as an integer of value 0 in memory.  Our C function definition of
`caml_hello_world` must therefore accept a single `value` parameter and return
a `value`.  Let's create the `hello_stubs.c` file now that implements this:

```c
#include <stdio.h>
#include <caml/mlvalues.h>

CAMLprim value
caml_hello_world(value v_unit)
{
  printf("Hello OCaml World from C!\n");
  return Val_unit;
}
```

You can now recompile the `hello` binary with this additional C file included
in the compiler command-line, and it should succeed:

```
$ ocamlopt -o hello hello.ml hello_stubs.c
$ ./hello
Hello OCaml World from C!
```

You must be *very* careful that the value you return from the C function
corresponds exactly to the memory representation of the types you declared
earlier in the `external` declaration of the ML file, or else heap carnage and
corruption will ensure.

<tip>
<title>Activating the debug runtime</title>

Despite your best efforts, it is easy to introduce a bug into C bindings that
cause heap invariants to be violated.  OCaml includes a variant of the runtime
library that is compiled with debugging symbols, and includes regular memory
integrity checks upon every garbage collection.  Running these often will abort
the program near the point of corruption and helps track it down quickly.

To use this, just recompile with `-runtime-variant d` set:

```
$ ocamlopt -runtime-variant d -verbose -o hello hello.ml hello_stubs.c
$ ./hello 
### OCaml runtime: debug mode ###
Initial minor heap size: 2048k bytes
Initial major heap size: 992k bytes
Initial space overhead: 80%
Initial max overhead: 500%
Initial heap increment: 992k bytes
Initial allocation policy: 0
Hello OCaml World!
```

If you get an error that `libasmrund.a` is not found, then this is probably
because you're using OCaml 4.00 and not 4.01.  It's only installed by default
in the very latest version, which you should be using via the `4.01.0dev+trunk`
OPAM switch.

</tip>

### Converting from OCaml values in C

The earlier hello world example is rather basic and only uses `unit` types.
Let's extend the signature to take a couple of `int` arguments instead of a
single `unit` so that we can send more useful data between OCaml and C:

```ocaml
external add_numbers: int -> int -> int = "caml_add_numbers"
let () = Printf.printf "From OCaml: %d\n" (add_numbers 10 15)
```

The `add_numbers` external function now takes two arguments, and returns an
integer instead of a simple `unit`.  The updated C stub looks like this:

```c
#include <stdio.h>
#include <caml/mlvalues.h>

CAMLprim value
caml_add_numbers(value v_arg1, value v_arg2)
{
  int v1 = Int_val(v_arg1);
  int v2 = Int_val(v_arg2);
  printf("From C:     %d + %d\n", v1, v2);
  return Val_int(v1+v2);
}
```

OCaml passes the integers to `caml_add_numbers` as `value` types, so the binding 
uses the `Int_val` macro to convert them into local stack variables. The `Int_val` macro
converts a `value` to an integer by removing the tag bit.  The C integers are then added
together and the result `value` is constructed by applying the
`Val_int` macro, which takes a C integer and tags it into becoming an OCaml
`value`.  When you compile and run this version of the code, you should see
this output:

```
$ ./hello 
From C:     10 + 15
From OCaml: 25
```

You should keep an eye out for warnings from your compiler which often indicate
that you've forgotten to correctly convert a `value`.  For example, try a broken
version of the previous binding:

```c
#include <stdio.h>
#include <caml/mlvalues.h>

CAMLprim value
caml_add_numbers(value v_arg1, value v_arg2)
{
  printf("From C:     %d + %d\n", v_arg1, v_arg2);
  return Val_int(v_arg1 + v_arg2);
}
```

The compiler will now complain of invalid format strings when you compile this:

```
$ ocamlopt -o hello -ccopt -Wall hello_stubs.c hello.ml
hello_stubs.c: In function caml_add_numbers:
hello_stubs.c:7: warning: format %d expects type int, but argument 2 has type value
hello_stubs.c:7: warning: format %d expects type int, but argument 3 has type value
hello_stubs.c:7: warning: format %d expects type int, but argument 2 has type value
hello_stubs.c:7: warning: format %d expects type int, but argument 3 has type value
$ ./hello 
From C:     21 + 31
From OCaml: 26
```

Notice that both the input and output integers are incorrect in the output,
since they still have their tag bits set when being added in the C code.  Don't
depend on the good graces of your C compiler to always spot such errors though.
It's also invalid to add two `value`s together without converting them into
native C type, but the C compiler can't warn about this and the result is
silently incorrect.  It's good practise to immediately convert arguments to
local C stack variables as early as possible, so you don't get the types mixed
up deep into the C function.

OCaml provides macros to convert to and from all the basic OCaml runtime values
and C types, of the form `to_from`.  For example `Val_long` means "Value from
long", and `Long_val` means "Long from value".  The table below summarises the
macros to extract various C types from OCaml `values` for 64-bit architectures.
Note that OCaml doesn't support a single-precision float, so these are always
double-precision.

TODO: buggy markdown below in table rendering

Macro                   OCaml Type         C type
-----                   ----------         ------
`Long_val`              `int`             `long`
`Int_val`               `int`             `int`
`Unsigned_long_val`     `int`             `unsigned long`
`Unsigned_int_val`      `int`             `unsigned int`
`Bool_val`              `bool`            `int`
`Double_val`            `float`           `double`
`String_val`            `string`          `string`
`Nativeint_val`         `Nativeint.t`     `long`
`Int32_val`             `Int32.t`         `long`
`Int64_val`             `Int64.t`         `unsigned long`

### Constructing OCaml values from C

Building OCaml values to return from C is a little more involved, since we
must ensure that any OCaml allocations aren't immediately cleaned up by the
garbage collector before they have been registered as live values.  Luckily,
OCaml provides some more macros to make this easier to enforce. 
Let's extend our earlier example to return a tuple of integers instead of just the result.

```ocaml
external add_numbers: int -> int -> int * int * int32 = "caml_add_numbers"
let () = 
  let (l,r,v) = add_numbers 10 15 in
  Printf.printf "From OCaml: %d+%d=%ld\n" l r v
```

The `add_numbers` external now returns a more complex data type that requires
allocating an OCaml tuple from within the C binding, storing the results within
the tuple, and returning that tuple.  The contents of the tuple are also a mix
of immediate values (the two first `int` fields in the tuple) and the last boxed `int32`
that also needs to be allocated on the OCaml heap.

```c
#include <stdio.h>
#include <caml/memory.h>
#include <caml/alloc.h>

CAMLprim value
caml_add_numbers(value v_arg1, value v_arg2)
{
  CAMLparam2(v_arg1, v_arg2);
  CAMLlocal1(v_res);
  int v1 = Int_val(v_arg1);
  int v2 = Int_val(v_arg2);
  printf("From C:     %d+%d=%d\n", v1, v2, (v1+v2));
  v_res = caml_alloc_tuple(3);
  Store_field(v_res, 0, Val_int(v1));
  Store_field(v_res, 1, Val_int(v2));
  Store_field(v_res, 2, caml_copy_int32(v1 + v2));
  CAMLreturn(v_res);
}
```

The stub now uses several new macros that are all defined in `caml/memory.h`.  The `CAMLparam` macro registers its parameters as _local roots_ with the garbage collector.  This ensures that if the garbage collector is triggered during the C binding, it will not relocate or free the value we just allocated.
We also need to allocate a tuple to store the result.  This is declared with the `CAMLlocal` macro, and returned via `CAMLreturn`.  For  many simple bindings, you can just follow the simple rule of replacing the C `return` with `CAMLreturn` and starting every function with `CAMLparam` and `CAMLlocal`, and not have to worry about the garbage collector.

<note>
<title>FFI rule: Use `CAMLparam` and `CAMLreturn` for OCaml values</title>

A function that has parameters or local variables of type value must begin with
a call to one of the `CAMLparam` macros and return with `CAMLreturn`,
`CAMLreturn0`, or `CAMLreturnT`. Local variables of type `value` must be
declared with one of the `CAMLlocal` macros. Arrays of values are declared with
`CAMLlocalN`. These macros must be used at the beginning of the function, not
in a nested block.

</note>

The tuple is then allocated via `caml_alloc_tuple`, with the number of fields representing the size of the tuple.  This must _immediately_ be followed by the `Store_field` macros to set the newly allocated tuple to sensible values.  In our example, we assign the first two fields to the local integers.  The `int32` requires another allocation, and  `caml_copy_int32` is used which copies a C `int32` into the correspondig OCaml `value`.  Once the tuple has been set, the function returns via `CAMLreturn`, which frees up the local roots that we registered at the beginning of the function.

<note>
<title>FFI rule: Use `Store_field` to assign to tuples, records and arrays</title>
Assignments to the fields of structured blocks must be done with the
`Store_field` macro (for normal blocks) or `Store_double_field` macro (for
arrays and records of floating-point numbers). Other assignments must not use
`Store_field` nor `Store_double_field`.

</note>

The `caml/alloc.h` header file lists all of the functions that allocate OCaml values.

Function name                       Argument                          OCaml return type
-------------                       --------                          -----------------
`caml_alloc_tuple (<len>)`          Number of fields                  Tuple or record
`caml_alloc_string (<len>)`         Size in bytes                     `string`
`caml_copy_string (char *)`         Pointer to a C string             `string`
`caml_copy_string_array (char **)`  Pointer to array of C strings     `string array`
`caml_copy_double (double)`         C `double` value                  `double`
`caml_copy_int32 (int32)`           32-bit C integer                  `int32`
`caml_copy_int64 (int64);           64-bit C integer                  `int64`
`caml_copy_nativeint (intnat);      native C integer size (`long`)    `Nativeint.t`

Tuples and records both have the same memory representation, with the same tag value (`0`).  This means that you can change how you map these fields into OCaml, without having to modify the C bindings. For example, our earlier integer addition example can also look like this:

```ocaml
type t = {
  v1: int;
  v2: int;
  res: int32;
}
 
external add_numbers: int -> int -> t = "caml_add_numbers"

let () = 
  let v = add_numbers 10 15 in
  Printf.printf "From OCaml: %d+%d=%ld\n" v.v1 v.v2 v.res
```

Records are never rearranged in memory, so the fields will appear in the same order they are declared. 

<note>
<title>Faster bindings for zero-allocation functions</title>

TODO Talk about "alloc" and "float" qualifiers to `external`

</note>
