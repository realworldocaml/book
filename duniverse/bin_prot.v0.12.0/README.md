Bin_prot - OCaml Type Converter for Binary Protocols
====================================================

What is `Bin_prot`?
-------------------

This library contains functionality for reading and writing OCaml-values in a
type-safe binary protocol.  It is extremely efficient, typically supporting
type-safe marshalling and unmarshalling of even highly structured values at
speeds sufficient to saturate a gigabit connection.  The protocol is also
heavily optimized for size, making it ideal for long-term storage of large
amounts of data.

The library is highly dependable and safe to use: a rigorous test suite has
to date guaranteed that this library has never exhibited a bug in production
systems in several years of use. `Bin_prot` has been successfully deployed in
mission-critical financial applications, storing many terabytes of structured
data derived from thousands of type definitions and typically processing
millions of messages a day in realtime for low-latency applications that
must not crash.

Since version two this library should work with all CPU architectures currently
supported by OCaml, no matter the word size (32 or 64 bit), alignment
requirements, or endianness.  Endianness defines the byte order in which
machine representations of integers (machine words) are stored in main memory.

`Bin_prot` provides users with a convenient and safe way of performing I/O
on any extensionally defined OCaml type (see later sections for details).
Functions, objects, first-class modules, as well as values whose type is
bound through a polymorphic record field are hence not supported.  This is
hardly ever a limitation in practice.

As of now, there is no support for cyclic or shared values.  Cyclic values
will lead to non-termination whereas shared values, besides requiring more
space when encoded, may lead to a substantial increase in memory footprint
when they are read back.  It would not be trivial to support these kinds
of values in a type-safe way without noticeably sacrificing performance.
If these kinds of values are needed, the user may want to use the as of
today still unsafe marshalling functions provided by OCaml.

This library uses the machine stack for efficiency reasons.  This can
potentially lead to a crash if the stack limit is reached.  Note that this
is also a limitation of the (unsafe) standard marshalling functions shipped
with OCaml.  This problem can happen for large amounts of data if recursion in
the type definition of the data structure is not limited to the last element.
Only in the latter case will tail-recursion allow for (practically) unlimited
amounts of data.  If this exceedingly rare limitation ever turned out to be
an issue in a user application, it can often be solved by redefining the data
type to allow for tail-recursion.  The limitation cannot be eliminated in
this library without significant performance impact and increased complexity.

Usage
-----

The API (`.mli`-files) in the `bin_prot` library directory (`lib`)
is fully documented, and HTML-documentation can be built from it on
installation.  The documentation for the latest release can also be found
[online](https://ocaml.janestreet.com/ocaml-core/latest/doc/bin_prot/Bin_prot/index.html).

Module `Common` defines some globally used types, functions, exceptions,
and values.  `Nat0` implements natural numbers including zero.

Modules `Read_ml` and `Write_ml` contain read and write functions respectively
for all basic types and are implemented in OCaml as far as reasonable.
Some operations are most easily performed in C.  If you only want to read
or write single, basic, unstructured values, using this module is probably
the most efficient and convenient way of doing this.

Otherwise you should annotate your type definitions to generate type
converters automatically (see later sections for details).  The preprocessor
in `syntax/pa_bin_prot.ml` will then generate highly optimized functions for
converting your OCaml-values to and from the binary representation.  This
automatically generated code will use functions in modules `Unsafe_common`,
`Unsafe_read_c` and `Unsafe_write_c`, which employ unsafe internal
representations to achieve optimal performance.  The auto-generated code is
extremely well-tested and should use these unsafe representations correctly.
Developers who want to make manual use of these unsafe calling conventions
for efficiency are strongly encouraged to test their code carefully.

The module `Size` allows you to compute the size of basic OCaml-values in the
binary representation before writing them to a buffer.  The code generator
will also provide you with functions for your user-defined types.

Module `Std` predefines converters for most standard OCaml types.  If you
use the preprocessor macros to generate code from type definitions, make sure
that the contents of this module is visible by e.g. adding the following at
the top of files using this library:

    :::ocaml
    open Bin_prot.Std

Note that you can shadow the definitions in the above module in the unlikely
event that the predefined ways of converting data are unsatisfactory to you.

The modules `Read_c` and `Write_c` wrap the unsafe low-level converters for
basic values to ones accessible safely from within OCaml and vice versa.  They
also export functions for wrapping user-defined converters.  This should help
developers make their converters available in the respective other
representation (low- or high-level).  The test applications in the distribution
use these wrappers to verify the correctness of implementations for low-level
(C) and high-level (OCaml) representations.

The module `Type_class` contains some extra definitions for type classes of
basic values.  These definitions can be passed to the function `bin_dump` in
module `Utils` to marshal values into buffers of exact size using the binary
protocol.  However, if bounds on the size of values are known, it is usually
more efficient to write them directly into preallocated buffers and just catch
exceptions if the buffer limits are unexpectedly violated.  Doing so should
never cause a crash.  That way one does not have to compute the size of the
value, which can sometimes be almost as expensive as writing the value in the
first place.

In module `Utils` the function `bin_read_stream` can be used to efficiently
read size-prefixed values as written by `bin_dump` with the `header` flag
set to `true`.  This module also offers several useful functors.  The ones
for `Binable` types help users create readers and writers if a type needs to
be converted to or from some intermediate representation before marshalling
or after unmarshalling respectively.  The functors for `Iterable` types are
helpful if some (usually abstract) data type offers iteration over elements and
if the series of iterated-over elements alone is sufficient to reconstruct the
original value.  This allows for a more compact protocol and for robustness
against changes to the internal representation of the data type (e.g. sets,
maps, etc.).

### Examples

Consider the following type definition:

    :::ocaml
    type t = A | B with bin_io

This will generate the functions `bin_size_t`, `bin_write_t`, and `bin_read_t`,
as well as the type class values `bin_writer_t`, `bin_reader_t`, and `bin_t`.
If you use the annotation `bin_write` instead of `bin_io`, then only the
write and size functions and their type class will be generated.  Specifying
`bin_read` will generate the read functions and associated type class only.
The annotation `bin_type_class` will generate the combined type class only,
thus allowing the user to easily define their own reader and writer type
classes.  The code generator may also generate low-level entry points used
for efficiency or backtracking.

The preprocessor can also generate signatures for conversion functions.  Just
add the wanted annotation to the type in a module signature for that purpose.

Specification of the Binary Protocol
------------------------------------

The binary protocol does not contain any data other than the minimum needed
to decode values.  This means that the user is responsible for e.g. writing
out the size of messages themselves if they want to be able to preallocate
sufficiently sized buffers before reading.  The `Utils` module provides some
simple functions for that matter, though users may obtain optimum efficiency
by managing buffers themselves.

Basic OCaml-values are written out character-wise as described below.
The specification uses hex codes to define the character encoding.  Some of
these values require size/length information to be written out before the
value (e.g. for lists, hash tables, strings, etc.).  Size information is
always encoded as natural numbers (`Nat0.t`).  The little-endian format is
used in the protocol for the contents of integers on all platforms.

The following definitions will be used in the encoding specifications below:

    :::text
    CODE_NEG_INT8  ->  0xff
    CODE_INT16     ->  0xfe
    CODE_INT32     ->  0xfd
    CODE_INT64     ->  0xfc

### Nat0.t

This type encodes natural numbers including zero.  It is frequently used by
`Bin_prot` itself for encoding size information for e.g. lists, arrays, etc.,
and hence defined first here.  Developers can reuse this type in their code,
too, of course.

If the value of the underlying integer is lower than a certain range,
this implies a certain encoding as provided on the right hand side of the
following definitions:

    :::text
    <  0x000000080  ->  lower 8 bits of the integer                     (1 byte)
    <  0x000010000  ->  CODE_INT16 followed by lower 16 bits of integer (3 bytes)
    <  0x100000000  ->  CODE_INT32 followed by lower 32 bits of integer (5 bytes)
    >= 0x100000000  ->  CODE_INT64 followed by all 64 bits of integer   (9 bytes)

The last line in the definitions above is only supported on 64 bit platforms
due to word size limitations.

Appropriate exceptions will be raised if there is an overflow, for example
if a 64 bit encoding is read on a 32 bit platform, or if the 32 bit or 64
bit encoding overflowed the 30 bit or 62 bit capacity of natural numbers
on their respective platforms.  The last kind of overflow is due to OCaml
reserving one bit for GC-tagging and the sign bit being lost.

### Unit values

    :::text
    ()  ->  0x00


### Booleans

    :::text
    false  ->  0x00
    true   ->  0x01

### Strings

First the length of the string is written out as a `Nat0.t`.  Then the
contents of the string is copied verbatim.

### Characters

Characters are written out verbatim.

### Integers

This includes all integer types: `int`, `int32`, `int64`, `nativeint`.
If the value is positive (including zero) and if it is:

    :::text
    <  0x00000080  ->  lower 8 bits of the integer                     (1 byte)
    <  0x00008000  ->  CODE_INT16 followed by lower 16 bits of integer (3 bytes)
    <  0x80000000  ->  CODE_INT32 followed by lower 32 bits of integer (5 bytes)
    >= 0x80000000  ->  CODE_INT64 followed by all 64 bits of integer   (9 bytes)

If the value is negative and if it is:

    :::text
    >= -0x00000080  ->  CODE_NEG_INT8 followed by lower 8 bits of integer (2 bytes)
    >= -0x00008000  ->  CODE_INT16 followed by lower 16 bits of integer   (3 bytes)
    >= -0x80000000  ->  CODE_INT32 followed by lower 32 bits of integer   (5 bytes)
    <  -0x80000000  ->  CODE_INT64 followed by all 64 bits of integer     (9 bytes)

All of the above branches will be considered when converting values of type
`int64`.  The case for `CODE_INT64` will only be considered with types `int`
and `nativeint` if the architecture supports it. `int32` will never be encoded
as a `CODE_INT64`.  Appropriate exceptions will be raised if the architecture
of or the type requested by the reader does not support some encoding or
if there is an overflow.  An overflow can only happen with values of type
`int`, because one bit is reserved by OCaml for the GC-tag again.

The reason for this peculiar encoding is of statistical nature.  It was
assumed that small or positive numbers are much more frequent in practice
than big or negative ones.  The code is biased accordingly to achieve good
compression and decoding performance.  For example, a positive integer in
the range from `0` to `127` requires only a single byte on the wire and only
a single branch to identify it.

### Floats

Floats are written out according to the 64 bit IEEE 754 floating point
standard, i.e. their memory representation is copied verbatim.

### References and lazy values

Same as the binary encoding of the value in the reference or of the lazily
calculated value.

### Option values

If the value is:

    :::text
    None    ->  0x00
    Some v  ->  0x01 followed by the encoding of v

### Tuples and records

Values in tuples and records are written out one after the other in the order
specified in the type definition.  Polymorphic record fields are supported
unless a value of the type bound by the field were accessed, which would
lead to an exception.

### Sum types

Each of the `n` tags in a sum type is assigned an integer from `0` to `n - 1`
in exactly the same order as they occur in the type.  If a value of this
type needs to be written out, then if:

    :::text
    n <= 256    ->  write out lower 8 bits of n  (1 byte)
    n <= 65536  ->  write out lower 16 bits of n (2 bytes)

Sum types with more tags are currently not supported and highly unlikely
to occur in practice.  Arguments to the tag are written out in the order
of occurrence.

### Polymorphic variants

The tags of these values are written out as four characters, more precisely
as the 32 bit hash value computed by OCaml for the given tag in little-endian
format.  Any arguments associated with the tag are written out afterwards
in the order of occurrence.

When polymorphic variants are being read, they will be matched in order
of occurrence (left-to-right) in the type and depth-first in the case of
included polymorphic types.  The first type containing a match for the
variant will be used for reading.  E.g.:

    :::ocaml
    type ab = [ `A | `B ] with bin_io
    type cda = [ `C | `D | `A ] with bin_io
    type abcda = [ ab | cda ] with bin_io

When reading type `abcda`, the reader associated with type `ab` rather than
`cda` will be invoked if a value of type ```A`` can be read.  This may not
make a difference in this example, but is important to know if the user
manually overrides converters.  It is strongly recommended to not merge
polymorphic variants if their readers might disagree about how to interpret
a certain tag.  This is inconsistent, confusing, and hard to debug.

### Lists and arrays

For lists and arrays the length is written out as a `Nat0.t` first, followed
by all values in the same order as in the data structure.

### Hash tables

First the size of the hash table is written out as a `Nat0.t`.  Then the
writer iterates over each binding in the hash table and writes out the key
followed by the value.

Note that this makes reading somewhat slower than if we used the internal
(extensional) representation of the hash table, because all values have to be
rehashed.  On the other hand, the format becomes more robust in case the hash
table implementation changes.  This has in fact already happened in practice
with the release of OCaml 4.00.  Users should take note of this and make
sure that all of their serialization routines remain future-proof by defining
wire formats that are independent of the implementation of abstract data types.

### Bigarrays of doubles (type `vec`) and characters (type `bigstring`)

First the dimension(s) are written out as `Nat0.t`.  Then the contents is
copied verbatim.

### Polymorphic values

There is nothing special about polymorphic values as long as there are
conversion functions for the type parameters.  E.g.:

    :::ocaml
    type 'a t = A | B of 'a with bin_io
    type foo = int t with bin_io

In the above case the conversion functions will behave as if `foo` had been
defined as a monomorphic version of `t` with `int` substituted for `'a`
on the right hand side.

### Abstract data types

If you want to convert an abstract data type that may impose constraints on the
well-formedness of values, you will have to roll your own conversion functions.
Use the functions in module `Read_c` and `Write_c` to map between low-level and
high-level representations, or implement those manually for maximum efficiency.
The `Utils` module may also come in handy as described in earlier sections,
e.g. if the value can be converted to and from an intermediate representation
that does not impose constraints, or if some sort of iteration is supported
by the data type.
