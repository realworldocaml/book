# Using Cstubs_structs

Ctypes is generally used to specify how to call C code using a DSL that is
executed at runtime.  This works great for functions but has some limitations
when it comes to data types and macros.  For example, one can define a C struct
using the Ctypes DSL, however the complete data type needs to be defined so that
Ctypes can calculate the proper size of the struct and the correct offsets of
members.

However, with some structs one may only want to access part of the struct, or it
might be large, or can change across OS and OS versions, or may be constructed
using compile-time tools such as macros.  In those cases, `Cstubs_structs`
provides a powerful tool to use C itself to generate the ML definition of the
struct that Ctypes can then use at runtime.  This definition will always be
correct.  Because C is being used to generate the definition, it also gives
access to other constructs that only exist at compile time, such as macros.

Using Cstubs_structs is a bit of a Rube Goldberg machine, however, no step is
superfluous.  The series of steps that will be needed are:

1. Write a stubs module that is a functor which defines the bindings.
2. Write a module that uses the bindings module and outputs a C file.
3. Compile the program from step 2 and execute it.
4. Compile the C program generated in step 3.
5. Run the C program from step 4, generating an ML module.
6. Compile the module generated in step 5.

The generated module can then be applied to the functor created in step 1.

# Example

The example program included in this tutorial shows how to partially define a
struct and access a macro.  The struct is `struct tm`, which is used in the time
API in C.  And the macro is `SHRT_MAX`, which defines the maximum value a value
of type `short` can hold.

## The Makefile

This tutorial contains a `Makefile` which builds the tutorial.  This is done
using a Makefile to make the steps clear and executable anywhere.

## Bindings

The file `bindings.ml` defines the functor for the stubs.  The struct is defined
in the module `Tm` and defines that the value `t` is the structure `tm`.  The
two fields defined are `tm_hour` and `tm_year`.  The actual `struct tm` has
several fields in its struct.  The strings are important in this case because
they will be what the generated C program references.

The module `Limits` defines the value `shrt_max` which corresponds to a constant
value of type `int`.  Again, the string is important because that is the name of
the macro which will be referenced.

## C Generator

The file `bindings_c_gen.ml` defines the ML program which will generate a C
source file when executed.  The important line is the one that calls
`Cstubs_structs.write_c`.  This takes the functor which will be applied as a
parameter.  This functor is applied to a module that generates the C program and
outputs it.

## Using it

The file `main.ml` defines a usage of the generated ML file.  It applies to the
`Bindings.Stubs` functor the module, `Bindings_stubs`, that was generated after
running the generated C program.  Accessing the struct and constant is the same
as if it were generated at runtime.

# A lot of steps, but not much code

The process of using `Cstubs_structs` is has several steps to it, however the
amount of code needed to do the whole thing is fairly small.  Most of the magic
is in the build process.  When binding data types, it might be a good idea to
use `Cstubs_structs` as the default tool, it is safer and less fragile then
defining a struct completely in Ocaml.

