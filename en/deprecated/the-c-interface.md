# Using the direct C interface

You should now understand the runtime structure of the OCaml heap and how the
compilation pipeline fits together.  This lets us move onto explaining how to
interface OCaml and C code directly using the direct interface it provides to
the OCaml heap.

OCaml defines an `external` keyword that maps OCaml functions to a C symbol.
When the function is invoked from OCaml, the C function will be called with the
OCaml function arguments using their native `value` representation. This
corresponds to the memory layout for OCaml values described earlier.

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
