# Memory Representation of Values

Much of the static type information contained within an OCaml program is
checked and discarded at compilation time, leaving a much simpler *runtime*
representation for values.  Understanding the runtime memory format is
important to write efficient programs that are compact in memory, and
understanding the results of profiling tools.

In this chapter, you'll learn:

* The architecture of the OCaml garbage collector.
* The layout of OCaml values as blocks within the major and minor heaps.
* The representation of the OCaml types within blocks.

This chapter is primarily informational, but it helps to know all this
to understand the compilation process in [xref](#compiler-output-formats),
handling external memory in [xref](#parsing-binary-protocols-with-bigarray),
or tracing and profiling your programs in [xref](#understanding-the-garbage-collector).
You might also want to interface with the OCaml C runtime directly instead of
using the simpler `ctypes` library described in
[xref](#foreign-function-interface).  This could be for performance reasons or a
more specialised embedded or kernel execution environment.

<note>
<title>Why do OCaml types disappear at runtime?</title>

The OCaml compiler runs through several phases during the compilation
process.  The first phase is syntax checking, during which source files are
parsed into Abstract Syntax Trees (ASTs).  The next stage is a *type checking*
pass over the AST.  In a validly typed program, a function cannot be applied
with an unexpected type.  For example, the `print_endline` function must
receive a single `string` argument, and an `int` will result in a type error.

Since OCaml verifies these properties at compile time, it doesn't need to keep
track of as much information at runtime. Thus, later stages of the compiler can
discard and simplify the type declarations to a much more minimal subset that's
actually required to distinguish polymorphic values at runtime.  This is a
major performance win versus something like a Java or .NET method call, where
the runtime must look up the concrete instance of the object and dispatch the
method call.  Those languages amortize some of the cost via "Just-in-Time"
dynamic patching, but OCaml prefers runtime simplicity instead.

We'll talk more about the compilation process in
[xref](#compiler-output-formats).

</note>

Let's start by explaining the garbage collector architecture, and then move
onto the memory layout of OCaml values.

## The garbage collector

A running OCaml program uses blocks of memory (i.e. contiguous sequences of
words in RAM) to represent many of the values that it deals with such as
tuples, records, closures or arrays.  An OCaml program implicitly allocates a
block of memory when such a value is created. 

```
# let x = { foo = 13; bar = 14 } ;;
```

An expression such as the record above requires a new block of memory with two
words of available space. One word holds the `foo` field and the second word
holds the `bar` field.  The OCaml compiler translates such an expression into
an explicit allocation for the block from OCaml's runtime system.

The OCaml runtime is a C library that provides a collection of routines that
can be called by running OCaml programs.  The runtime manages a *heap*, which
is a collection of memory regions it obtains from the operating system using
*malloc(3)*. The OCaml runtime uses these memory regions to hold *heap blocks*,
which it fills up in response to allocation requests by the OCaml program.

### The mark and sweep GC strategy

When there isn't enough memory available to satisfy an allocation request from
the already-allocated heap blocks, the runtime system invokes the *garbage
collector* (or GC). An OCaml program does not explicitly free a heap block when
it is done with it. The GC determines which heap blocks are "live" and which
heap blocks are "dead", i.e. no longer in use. Dead blocks are collected and
their memory made available for re-use by the application.

The garbage collector does not keep constant track of blocks as they are
allocated and used.  Instead, it regularly scans blocks by starting from a set
of *roots*, which are values that the application always has access to (such as
the stack).  The GC maintains a directed graph in which heap blocks are nodes.
There is an edge from heap block `b1` to heap block `b2` if some field of `b1`
points to `b2`.  All blocks reachable from the roots by following edges in the
graph must be retained, and unreachable blocks can be reused by the
application.  This strategy is commonly known as "mark and sweep" collection.

### Generational garbage collection

The typical OCaml programming style typically involves allocating many small
blocks of memory that are used for a short period of time and then never
accessed again.  OCaml takes advantage of this fact to improve performance by
using a *generational* garbage collector.

A generational GC keeps separate memory regions to hold blocks based on how long
the blocks have been live.  OCaml's heap is split in two:

* a small, fixed-size *minor heap* where most most blocks are initially allocated
* a large, variable-sized *major heap* for blocks that have been live longer or are
  larger than 4KB.

A typical functional programming style means that young blocks tend to die
young, and old blocks tend to stay around for longer than young ones.  This is
often referred to as the *generational hypothesis*. 

OCaml uses different memory layouts and garbage collection algorithms for the
major and minor heaps to account for this generational difference.  We'll
explain the two mechanisms next.

### The fast minor heap

The minor heap is one contiguous chunk of virtual memory containing a sequence
of OCaml blocks.  If there is space, allocating a new block is a fast
constant-time operation in which the pointer to the end of the heap is
incremented by the desired size.

To garbage collect the minor heap, OCaml uses *copying collection* to copy all
live blocks in the minor heap to the major heap.  This takes work proportional
to the number of live blocks in the minor heap, which is typically small
according to the generational hypothesis.

One complexity of generational collection arises from the fact that minor heap
sweeps are much more frequent than major heap collections. In order to know
which blocks in the minor heap are live, the collector must track which
minor-heap blocks are directly pointed to by major-heap blocks.  Without this
information, the minor collection would require scanning the much larger major
heap.

OCaml maintains a set of such *inter-generational pointers*, and the compiler
introduces a write barrier to update this set whenever a major-heap block is
modified to point at a minor-heap block. We'll talk more about the implications
of the write barrier in [xref](#understanding-the-garbage-collector).

### The long-lived major heap

The major heap consists of any number of non-contiguous chunks of virtual
memory, each containing live blocks interspersed with regions of free memory.
The runtime system maintains a free-list data structure that indexes all the
free memory. This list is used to satisfy allocation requests for OCaml blocks.
The major heap is typically much larger than the minor heap, and is cleaned via
a mark-and-sweep garbage collection algorithm.

* The *mark* phase traverses the block graph and marks all live blocks by setting a bit in the tag of the block header (known as the *color* tag).
* The *sweep* phase sequentially scans the heap chunks and identifies dead blocks that weren't marked earlier.
* The *compact* phase moves live blocks to eliminate the gaps of free memory into a freshly allocated heap. This prevents heap blocks fragmenting in long-lived programs.

A major heap garbage collection must *stop the world* (that is, halt the
application) in order to ensure that blocks can be safely moved around without
this move being observed by the live application. The mark-and-sweep phases run
incrementally over slices of memory to avoid pausing the application for long
periods of time.  Only the compaction phase touches all the memory in one go,
and is a relatively rare operation.

The `Gc` module lets you control all these parameters from your application.
Although the defaults are usually sensible, understanding them and tuning them
is an important concern we will discuss later in [xref](#understanding-the-garbage-collector).

## The representation of OCaml values

So far, we've described the overall memory layout, but not what the contents of
each block contains.  Every OCaml variable points to a `value`
at runtime, which is a single memory word that is either an integer or a
pointer.  The OCaml runtime needs to understand the difference between the two so
that it can follow pointers, but not integers (which directly hold the value
and don't point to anything meaningful).

If the lowest bit of the block word is non-zero, the value is an unboxed
integer.  Several OCaml types map onto this integer representation, including
`bool`, `int`, the empty list, `unit`, and variants without constructors.
Integers are the only unboxed runtime values in OCaml, which means that they
can be stored directly without having to allocate a wrapper structure that will
take up more memory. They can also be passed directly to other function calls
in registers, and so are generally the cheapest and fastest values to use in
OCaml.

If the lowest bit of the `value` is zero, then the value is a memory pointer.
A pointer value is stored unmodified, since pointers are guaranteed to be
word-aligned and so the bottom bits are always zero. If the pointer is inside a
memory chunk that is marked as being managed by the OCaml runtime, it is
assumed to point to an OCaml block (see below).  If it points outside the OCaml
runtime area, it is is treated as an opaque C pointer to some other system
resource.

<note>
<title>Some history about OCaml's word-aligned pointers</title>

The alert reader may be wondering how OCaml can guarantee that all of its
pointers are word-aligned.  In the old days when RISC chips such as Sparc, MIPS
and Alpha were commonplace, unaligned memory accesses were forbidden by the
instruction set architecture and would result in a CPU exception that
terminated the program.  Thus, all pointers were historically rounded off to
the architecture word-size (usually 32- or 64-bits).

Modern CISC processors such as the Intel x86 do support unaligned memory accesses,
but the chip still runs faster if accesses are word-aligned.  OCaml therefore
simply mandates that all pointers be word-aligned, which guarantees that the
bottom few bits of any valid pointer will be zero.  Setting the bottom bit to
a non-zero value is a simple way to mark an integer, at the cost of losing
that single bit of precision.

An even more alert reader will be wondering about the performance implications
are for integer arithmetic using this tagged representation.  Since the bottom
bit is set, any operation on the integer has to shift the bottom bit right to
recover the "native" value.  The native code OCaml compiler generates very
efficient x86 assembly code in this case, and takes advantage of modern
processor instructions to either hide the extra work, or get it for free from
the instruction set.  Addition and substraction are a single instruction, and
multiplication is only a few more.

</note>

### Blocks and values

An OCaml *block* is the basic unit of allocation on the heap.  A block consists
of a one-word header (either 32- or 64-bits) followed by variable-length data
that is either opaque bytes or an array of *fields*.  The header has a
multi-purpose tag byte that defines whether to interprete the subsequent data
as opaque or OCaml fields.  The garbage collector never inspects opaque bytes,
but the array of fields are all treated as more valid OCaml values. The garbage
collector always inspects fields, and follows them as part of the collection
process described earlier.

```
+------------------------+-------+----------+----------+----------+----
| size of block in words |  col  | tag byte | value[0] | value[1] | ...
+------------------------+-------+----------+----------+----------+----
 <-either 22 or 54 bits-> <2 bit> <--8 bit-->
```

The size field records the length of the block in memory words. Note that it is
limited to 22-bits on 32-bit platforms, which is the reason why OCaml strings
are limited to 16MB on that architecture. If you need bigger strings, either
switch to a 64-bit host, or use the `Bigarray` module described in [xref](#parsing-binary-protocols-with-bigarray).  The
2-bit color field is used by the garbage collector to keep track of its
state during mark-and-sweep, and is not exposed directly to OCaml programs.

Tag Color   Block Status
---------   ------------
blue        on the free list and not currently in use
white       not reached yet, but possibly reachable
gray        reachable, but its fields have not been scanned
black       reachable, and its fields have been scanned

A block's tag byte is multi-purpose, and indicates whether the data array
represents opaque bytes or fields.  If a block's tag is greater than or equal
to `No_scan_tag` (251), then the block's data are all opaque bytes, and are not
scanned by the collector. The most common such block is the `string` type,
which we describe more below.

The exact representation of values inside a block depends on their OCaml type.
They are summarised in the table below, and then we'll examine some of them in
greater detail.

OCaml Value                        Representation
-----------                        --------------
any `int` or `char`                directly as a value, shifted left by 1 bit, with the least significant bit set to 1
`unit`, `[]`, `false`              as OCaml `int` 0.
`true`                             as OCaml `int` 1.
`Foo | Bar`                        as ascending OCaml `int`s, starting from 0.
`Foo | Bar of int`                 variants with parameters are boxed, while entries with no parameters are unboxed (see below).
polymorphic variants               variable space usage depending on the number of parameters (see below).
floating point number              as a block with a single field containing the double-precision float.
string                             word-aligned byte arrays that are also directly compatible with C strings.
`[1; 2; 3]`                        as `1::2::3::[]` where `[]` is an int, and `h::t` a block with tag 0 and two parameters.
tuples, records and arrays         an array of values. Arrays can be variable size, but structs and tuples are fixed size.
records or arrays, all float       special tag for unboxed arrays of floats. Doesn't apply to tuples.

### Integers, characters and other basic types

Many basic types are stored directly as unboxed values at runtime.  The native
`int` type is the most obvious, although it drops a single bit of precision due
to the tag bit described earlier. Other atomic types such as the `unit` and
empty list `[]` value are stored as constant integers.  Boolean values have a
value of `0` and `1` for `true` and `false` respectively.

Remember that since integers are never allocated on the heap, all of these
basic types such as empty lists and `unit` are therefore very efficient to use.
They will often be passed in registers and never even appear on the stack,
if you don't have too many parameters to your functions or are running on
a modern architecture with lots of spare registers (such as `x86_64`).

### Tuples, records and arrays

```
+---------+----------+----------- - - - - 
| header  | value[0] | value[1] | ....
+---------+----------+----------+- - - - -
```

Tuples, records and arrays are all represented identically at runtime as a
block with tag `0`.  Tuples and records have constant sizes determined at
compile-time, whereas arrays can be of variable length.  While arrays are
restricted to containing a single type of element in the OCaml type system,
this is not required by the memory representation.

You can check the difference between a block and a direct integer yourself
using the `Obj` module, which exposes the internal representation of values to
OCaml code.

```ocaml
# Obj.is_block (Obj.repr (1,2,3)) ;;
- : bool = true
# Obj.is_block (Obj.repr 1) ;;
- : bool = false
```

The `Obj.repr` function retrieves the runtime representation of any OCaml
value.  `Obj.is_block` checks the bottom bit to determine if the value is
a block header or an unboxed integer.

### Floating point numbers and arrays

Floating point numbers in OCaml are always stored as full double-precision
values.  Individual floating point values are stored as a block with a single
field that contains the number.  This block has the `Double_tag` set which
signals to the collector that the floating point value is not to be scanned.

```ocaml
# Obj.tag (Obj.repr 1.0) = Obj.double_tag ;;
- : int = 253
# Obj.double_tag ;;
- : int = 253
```ocaml

Since each floating-point value is boxed in a separate memory block, it can be
inefficient to handle large arrays of floats in comparison to unboxed integers.
OCaml therefore special-cases records or arrays that contain *only* `float`
types. These are stored in a block that contains the floats packed directly in
the data section, with the `Double_array_tag` set to signal to the collector
that the contents are not OCaml values.

```
+---------+----------+----------- - - - - 
| header  | float[0] | float[1] | ....
+---------+----------+----------+- - - - -
```

You can test this for yourself using the `Obj.tag` function to check that the
allocated block has the expected runtime tag, and `Obj.double_field` to
retrieve a float from within the block.

```ocaml
# open Obj ;;
# tag (repr [| 1.0; 2.0; 3.0 |]) ;;
- : int = 254
# tag (repr (1.0, 2.0, 3.0) ) ;;
- : int = 0 
# double_field (repr [| 1.1; 2.2; 3.3 |] ) 1 ;;
- : float = 2.2
# Obj.double_field (Obj.repr 1.234) 0;;
- : float = 1.234
```ocaml

Notice that float tuples are *not* optimized in the same way as float records
or arrays, and so they have the usual tuple tag value of `0`. Only records
and arrays can have the array optimization, and only if every single field is
a float.

### Variants and lists

Basic variant types with no extra parameters for any of their branches are
simply stored as an OCaml integer, starting with `0` for the first option and
in ascending order. 

```ocaml
# open Obj ;;
# type t = Apple | Orange | Pear ;;
type t = Apple | Orange | Pear
# ((magic (repr Apple)) : int) ;;
- : int = 0
# ((magic (repr Pear)) : int) ;;
- : int = 2
# is_block (repr Apple) ;;
- : bool = false
```

`Obj.magic` unsafely forces a type cast between any two OCaml types; in this
example the `int` type hint retrieves the runtime integer value. The
`Obj.is_block` confirms that the value isn't a more complex block, but just an
OCaml `int`.

Variants that have parameters arguments are a little more complex.  They are
stored as blocks, with the value *tags* ascending from 0 (counting from
leftmost variants with parameters).  The parameters are stored as words in the
block.

```ocaml
# type t = Apple | Orange of int | Pear of string | Kiwi ;;
type t = Apple | Orange of int | Pear of string | Kiwi
# is_block (repr (Orange 1234)) ;;
- : bool = true
# tag (repr (Orange 1234)) ;; 
- : int = 0
# tag (repr (Pear "xyz")) ;;
- : int = 1
# (magic (field (repr (Orange 1234)) 0) : int) ;;
- : int = 1234
(magic (field (repr (Pear "xyz")) 0) : string) ;;
- : string = "xyz"
```

In the above example, the `Apple` and `Kiwi` values are still stored as normal
OCaml integers with values `0` and `1` respectively.  The `Orange` and `Pear`
values both have parameters, and are stored as blocks whose tags ascend from
`0` (and so `Pear` has a tag of `1`, as the use of `Obj.tag` verifies).
Finally, the parameters are fields which contain OCaml values within the block,
and `Obj.field` can be used to retrieve them.

Lists are stored with a representation that is exactly the same as if the list
was written as a variant type with `Head` and `Cons`.  The empty list `[]` is
an integer `0`, and subsequent blocks have tag `0` and two parameters: a block
with the current value, and a pointer to the rest of the list.

<warning>
<title>`Obj` module considered harmful</title>

The `Obj` module is an undocumented module that exposes the internals of the
OCaml compiler and runtime.  It is very useful for examining and understanding
how your code will behave at runtime, but should *never* be used for production
code unless you understand the implications.  The module bypasses the OCaml
type system, making memory corruption and segmentation faults possible.

Some theorem provers such as Coq do output code which uses `Obj` internally,
but the external module signatures never expose it.  Unless you too have a
machine proof of correctness to accompany your use of `Obj`, stay away from it
except for debugging!

</warning>

Due to this encoding, there is a limit around 240 variants with parameters that
applies to each type definition, but the only limit on the number of variants
without parameters is the size of the native integer (either 31- or 63-bits).
This limit arises because of the size of the tag byte, and that some of the
high numbered tags are reserved.

### Polymorphic variants

Polymorphic variants are more flexible than normal variants when writing code,
but can be less efficient at runtime. This is because there isn't as much
static compile-time information available to optimise their memory layout.
This isn't always the case, however.  A polymorphic variant without any
parameters is stored as an unboxed integer and so only takes up one word of
memory. Unlike normal variants, the integer value is determined by apply a hash
function to the *name* of the variant.  The hash function isn't exposed
directly by the compiler, but the `type_conv` library from Core provides an
alternative implementation.

```ocaml
# #require "type_conv" ;;
# Pa_type_conv.hash_variant "Foo" ;;
- : int = 3505894
# (Obj.magic (Obj.repr `Foo) : int) ;;
- : int = 3505894
```

The hash function is designed to give the same results on 32-bit and 64-bit
architectures, so the memory representation is stable across different CPUs and
host types.

Polymorphic variants use more memory space when parameters are included in the
datatype constructors.  Normal variants use the tag byte to encode the variant
value, but this byte is insufficient to encode the hashed value for polymoprhic
variants.  Therefore, they must allocate a new block (with tag `0`) and store
the value in there instead. This means that polymorphic variants with
constructors use one word of memory more than normal variant constructors.

Another inefficiency is when a polymorphic variant constructor has more than
one parameter.  Normal variants hold parameters as a single flat block with
multiple fields for each entry, but polymorphic variants must adopt a more
flexible uniform memory representation since they may be re-used in a different
context. They allocate a tuple block for the parameters that is pointed to from
the argument field of the variant. Thus, there are three additional words for
such variants, along with an extra memory indirection due to the tuple.

### String values

Strings are standard OCaml blocks with the header size defining the size
of the string in machine words. The `String_tag` (252) is higher than the
`No_scan_tag`, indicating that the contents of the block are opaque to the
collector.  The block contents are the contents of the string, with padding
bytes to align the block on a word boundary.

```ocaml
+---------------+----------------+--------+-----------+
| header        | 'a' 'b' 'c' 'd' 'e' 'f' | '\O' '\1' |
+---------------+----------------+--------+-----------+
                L data                    L padding
```

On a 32-bit machine, the padding is calculated based on the modulo of the
string length and word size to ensure the result is word-aligned.  A
64-bit machine extends the potential padding up to 7 bytes instead of 3.

String length mod 4  Padding
-------------------  -------
0                    `00 00 00 03`
1                    `00 00 02`
2                    `00 01`
3                    `00`

This string representation is a clever way to ensure that the string contents
are always zero-terminated by the padding word, and still compute its length
efficiently without scanning the whole string.  The following formula is used:

```
number_of_words_in_block * sizeof(word) - last_byte_of_block - 1
```

The guaranteed NULL-termination comes in handy when passing a string to C, but
is not relied upon to compute the length from OCaml code. Thus, OCaml strings
can contain null bytes at any point within the string, but care should be taken
that any C library functions can also cope with this.

### Custom heap blocks

OCaml supports *custom* heap blocks via a `Custom_tag` that let the runtime
perform user-defined operations over OCaml values.  A custom block lives in the
OCaml heap like an ordinary block and can be of whatever size the user desires.
The `Custom_tag` (255) is higher than `No_scan_tag` and isn't scanned by the
garbage collector.  This means that it cannot contain any OCaml values, but
is useful to track pointers into the external C heap.

The first word of the data within the custom block is a C pointer to a `struct`
of custom operations. The custom block cannot have pointers to OCaml blocks and
is opaque to the garbage collector.

```c
struct custom_operations {
  char *identifier;
  void (*finalize)(value v);
  int (*compare)(value v1, value v2);
  intnat (*hash)(value v);
  void (*serialize)(value v,
                    /*out*/ uintnat * wsize_32 /*size in bytes*/,
                    /*out*/ uintnat * wsize_64 /*size in bytes*/);
  uintnat (*deserialize)(void * dst);
  int (*compare_ext)(value v1, value v2);
};
```

The custom operations specify how the runtime should perform polymorphic
comparison, hashing and binary marshalling.  They also optionally contain a
*finalizer* that the runtime calls just before the block is garbage collected.
This finalizer has nothing to do with ordinary OCaml finalizers (as created by
`Gc.finalise` and explained in [xref](tuning-the-runtime)).  Instead, they are
used to call C cleanup functions such as `free`.

When a custom block is allocated, you can also specify the proportion of
"extra-heap resources" consumed by the block, which will affect the garbage
collector's decision as to how much work to do in the next major slice.
(_avsm_: TODO elaborate on this or move to the C interface section)
