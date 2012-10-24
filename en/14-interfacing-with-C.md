# Understanding the runtime system

Much of the static type information contained within an OCaml program is
checked and discarded at compilation time, leaving a much simpler *runtime*
representation for values.  Understanding this difference is important for
writing efficient programs, and also for interfacing with C libraries that work
directly with the runtime system.

Let's start by explaining the memory layout, and then move onto the details
of how C bindings work.

## The garbage collector

A running OCaml program uses blocks of memory (i.e. contiguous sequences of
words in RAM) to represent many of the values that it deals with such as
tuples, records, closures or arrays.  An OCaml program implicitly allocates a
block of memory when such a value is created. 

~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let x = { foo = 13; bar = 14 } ;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~

An expression such as the record above requires a new block of memory with two
words of available space. One word holds the `foo` field and the second word
holds the `bar` field.  The OCaml compiler translates such an expression into
an explicit allocation for the block from OCaml's runtime system: a C library
that provides a collection of routines that can be called by running OCaml
programs.  The runtime system manages a *heap*, which a collection of memory
regions it obtains from the operating system using *malloc(3)*. The OCaml runtime
uses these memory regions to hold *heap blocks*, which it then fills up in
response to allocation requests by the OCaml program.

When there is'nt enough memory available to satisfy an allocation request from
the allocated heap blocks, the runtime system invokes the *garbage collector*
(or GC). An OCaml program does not explicitly free a heap block when it is done
with it, and the GC must determine which heap blocks are "alive" and which heap
blocks are *dead*, i.e. no longer in use. Dead blocks are collected and their
memory made available for re-use by the application.

The garbage collector does not keep constant track of blocks as they are
allocated and used.  Instead, it regularly scans blocks by starting from a set
of *roots*, which are values that the application always has access to (such as
the stack).  The GC maintains a directed graph in which heap blocks are
nodes, and there is an edge from heap block `b1` to heap block `b2` if some
field of `b1` points to `b2`.  All blocks reachable from the roots by following
edges in the graph must be retained, and unreachable blocks can be reused.

With the typical OCaml programming style, many small blocks are frequently
allocated, used for a short period of time, and then never used again.  OCaml
takes advantage of this fact to improve the performance of allocation and
collection by using a *generational* garbage collector. This means that it has
different memory regions to hold blocks based on how long the blocks have been
alive.  OCaml's heap is split in two; there is a small, fixed-size *minor heap*
used for initially allocating most blocks, and a large, variable-sized *major
heap* for holding blocks that have been alive longer or are larger than 4KB.  A
typical functional programming style means that young blocks tend to die young,
and old blocks tend to stay around for longer than young ones (this is referred
to as the *generational hypothesis*). To reflect this, OCaml uses different
memory layouts and garbage collection algorithms for the major and minor heaps.

### The fast minor heap

The minor heap is one contiguous chunk of memory containing a sequence of heap
blocks that have been allocated.  If there is space, allocating a new block is
a fast constant-time operation in which the pointer to the end of the heap is
incremented by the desired size.  To garbage collect the minor heap, OCaml uses
*copying collection* to copy all live blocks in the minor heap to the major
heap.  This only takes work proportional to the number of live blocks in the
minor heap, which is typically small according to the generational hypothesis.

One complexity of generational collection is that in order to know which blocks
in the minor heap are live, the collector must know which minor-heap blocks are
directly pointed to by major-heap blocks.  To do this, OCaml maintains a set of
such inter-generational pointers, and, through cooperation with the compiler,
uses a write barrier to update this set whenever a major-heap block is modified
to point at a minor-heap block.

### The long-lived major heap

The major heap consists of a number of chunks of memory, each containing live
blocks interspersed with regions of free memory.  The runtime system maintains
a free list data structure that indexes all the free memory, and this list is
used to satisfy allocation requests. OCaml uses mark and sweep garbage
collection for the major heap.  The *mark* phase to traverses the block graph
and marks all live blocks by setting a bit in the color tag of the block header.
(_avsm_: we only explain the color tag in the next section, so rephrase or xref).

The *sweep* phase sequentially scans all heap memory and identifies dead blocks
that weren't marked earlier.  The *compact* phase relocates live blocks to
eliminate the gaps of free memory between them and ensure memory does not
fragment.

A garbage collection must *stop the world* (that is, halt the application) in
order to ensure that blocks can be safely moved. The mark and sweep phases run
incrementally over slices of memory, and are broken up into a number of steps
that are interspersed with the running OCaml program.  Only a compaction
touches all the memory in one go, and is a relatively rare operation.

The `Gc` module lets you control all these parameters from your application,
and we will discuss garbage collection tuning in (_avsm_: crossref).

## The representation of values

Every OCaml *value* is a single word that is either an integer or a pointer.
If the lowest bit of the word is non-zero, the value is an unboxed integer.
Several OCaml types map onto this integer representation, including `bool`,
`int`, the empty list, `unit`, and variants without constructors.  Integers are
the only unboxed runtime values in OCaml, and are the cheapest values to
allocate.

If the lowest bit of the `value` is zero, then the value is a pointer.  A
pointer value is stored unmodified, since pointers are guaranteed to be
word-aligned and the bottom bits are always zero. If the pointer is inside an
area managed by the OCaml runtime, it is assumed to point to an OCaml *block*.
If it points outside the OCaml runtime area, it is is treated as an opaque C
pointer to some other system resource.

### Blocks and values

An OCaml *block* is the basic unit of allocation on the heap.  A block consists
of a one-word header (either 32- or 64-bits) followed by variable-length data,
which is either opaque bytes or *fields*.  The collector never inspects opaque
bytes, but fields are valid OCaml values. The runtime always inspects fields,
and follows them as part of the garbage collection process described earlier.
Every block header has a multipurpose tag byte that defines whether to
interprete the subsequent data as opaque or OCaml fields.

(_avsm_: pointers to blocks actually point 4/8 bytes into it, for some efficiency
reason that I cannot recall right now).

~~~~~~~~~~~~~~~~~~~~~~~~~~~
+------------------------+-------+----------+----------+----------+----
| size of block in words |  col  | tag byte | value[0] | value[1] | ...
+------------------------+-------+----------+----------+----------+----
 <-either 22 or 54 bits-> <2 bit> <--8 bit-->
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The size field records the length of the block in memory words. Note that it is
limited to 22-bits on 32-bit platforms, which is the reason why OCaml strings
are limited to 16MB on that architecture. If you need bigger strings, either
switch to a 64-bit host, or use the `Bigarray` module (_avsm_: xref).  The
2-bit color field is used by the garbage collector to keep track of its
status, and is not exposed directly to OCaml programs.

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

(_avsm_: too much info here) If the header is zero, then the object has been
forwarded as part of minor collection, and the first field points to the new
location.  Also, if the block is on the `oldify_todo_list`, part of the minor
gc, then the second field points to the next entry on the oldify_todo_list.

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

<note>
<title>Why do OCaml types disappear at runtime?</title>

The OCaml compiler runs through several phases of during the compilation
process.  After syntax checking, the next stage is *type checking*.  In a
validly typed program, a function cannot be applied with an unexpected type.
For example, the `print_endline` function must receive a single `string`
argument, and an `int` will result in a type error.

Since OCaml verifies these properties at compile time, it doesn't need to keep
track of as much information at runtime. Thus, later stages of the compiler can
discard and simplify the type declarations to a much more minimal subset that's
actually required to distinguish polymorphic values at runtime.  This is a
major performance win versus something like a Java or .NET method call, where
the runtime must look up the concrete instance of the object and dispatch the
method call.  Those languages amortize some of the cost via "Just-in-Time"
dynamic patching, but OCaml prefers runtime simplicity instead.

</note>

### Integers, characters and other basic types

Many basic types are stored directly as unboxed values at runtime.  The native
`int` type is the most obvious, although it drops a single bit of precision due
to the tag bit described earlier. Other atomic types such as the `unit` and
empty list `[]` value are stored as constant integers.  Boolean values have a
value of `0` and `1` for `true` and `false` respectively.

<note>
<title>Why are OCaml integers missing a bit?</title>

Since the lowest bit of an OCaml value is reserved, native OCaml integers have
a maximum allowable length of 31- or 63-bits, depending on the host
architecture. The rationale for reserving the lowest bit is for efficiency.
Pointers always point to word-aligned addresses, and so their lower bits are
normally zero. By setting the lower bit to a non-zero value for integers, the
garbage collector can simply iterate over every header tag to distinguish
integers from pointers.  This reduces the garbage collection overhead on the
overall program.

</note>

(_avsm_: explain that integer manipulation is almost as fast due to isa quirks)

### Tuples, records and arrays

~~~~~~~~~~~~~~~~~~~~~~~~~~~
+---------+----------+----------- - - - - 
| header  | value[0] | value[1] | ....
+---------+----------+----------+- - - - -
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Tuples, records and arrays are all represented identically at runtime, with a
block with tag `0`.  Tuples and records have constant sizes determined at
compile-time, whereas arrays can be of variable length.  While arrays are
restricted to containing a single type of element in the OCaml type system,
this is not required by the memory representation.

You can check the difference between a block and a direct integer yourself
using the `Obj` module, which exposes the internal representation of values to
OCaml code.

~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Obj.is_block (Obj.repr (1,2,3)) ;;
- : bool = true
# Obj.is_block (Obj.repr 1) ;;
- : bool = false
~~~~~~~~~~~~~~~~

The `Obj.repr` function retrieves the runtime representation of any OCaml
value.  `Obj.is_block` checks the bottom bit to determine if the value is
a block header or an unboxed integer.

### Floating point numbers and arrays

Floating point numbers in OCaml are always stored as full double-precision
values.  Individual floating point values are stored as a block with a single
field that contains the number.  This block has the `Double_tag` set which
signals to the collector that the floating point value is not to be scanned.

~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Obj.tag (Obj.repr 1.0) = Obj.double_tag ;;
- : int = 253
# Obj.double_tag ;;
- : int = 253
~~~~~~~~~~~~~~~~ { .ocaml-toplevel }

Since each floating-point value is boxed in a separate memory block, it can be
inefficient to handle large arrays of floats in comparison to unboxed integers.
OCaml therefore special-cases records or arrays that contain *only* `float`
types. These are stored in a block that contains the floats packed directly in
the data section, with the `Double_array_tag` set to signal to the collector
that the contents are not OCaml values.

~~~~~~~~~~~~~~~~~~~~~~~~~~~
+---------+----------+----------- - - - - 
| header  | float[0] | float[1] | ....
+---------+----------+----------+- - - - -
~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can test this for yourself using the `Obj.tag` function to check that the
allocated block has the expected runtime tag, and `Obj.double_field` to
retrieve a float from within the block.

~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# open Obj ;;
# tag (repr [| 1.0; 2.0; 3.0 |]) ;;
- : int = 254
# tag (repr (1.0, 2.0, 3.0) ) ;;
- : int = 0 
# double_field (repr [| 1.1; 2.2; 3.3 |] ) 1 ;;
- : float = 2.2
# Obj.double_field (Obj.repr 1.234) 0;;
- : float = 1.234
~~~~~~~~~~~~~~~~ { .ocaml-toplevel }

Notice that float tuples are *not* optimized in the same way as float records
or arrays, and so they have the usual tuple tag value of `0`. Only records
and arrays can have the array optimization, and only if every single field is
a float.

### Variants and lists

Basic variant types with no extra parameters for any of their branches are
simply stored as an OCaml integer, starting with `0` for the first option and
in ascending order. 

~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# open Obj ;;
# type t = Apple | Orange | Pear ;;
type t = Apple | Orange | Pear
# ((magic (repr Apple)) : int) ;;
- : int = 0
# ((magic (repr Pear)) : int) ;;
- : int = 2
# is_block (repr Apple) ;;
- : bool = false
~~~~~~~~~~~~~~~~

`Obj.magic` unsafely forces a type cast between any two OCaml types; in this
example the `int` type hint retrieves the runtime integer value. The
`Obj.is_block` confirms that the value isn't a more complex block, but just an
OCaml `int`.

Variants that have parameters arguments are a little more complex.  They are
stored as blocks, with the value *tags* ascending from 0 (counting from
leftmost variants with parameters).  The parameters are stored as words in the
block.

~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
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
~~~~~~~~~~~~~~~~

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

~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# #require "type_conv" ;;
# Pa_type_conv.hash_variant "Foo" ;;
- : int = 3505894
# (Obj.(magic (repr `Foo)) : int) ;;
- : int = 3505894
~~~~~~~~~~~~~~~~

The hash function is designed to give the same results on 32-bit and 64-bit
architectures, so the memory representation is stable across different CPUs and
host types.

Polymorphic variants use more memory space when parameters are included in the
datatype constructors.  Normal variants use the tag byte to encode the variant
value, but this byte is insufficient to encode the hashed value for polymoprhic
variants.  Therefore, they must allocate a new block (with tag `0`) and store
the value in there instead. This means that polymorphic variants with
constructors use one word of memory more than normal variant constructors.

Another inefficiency is when a polymorphic variant construct has more than one
parameter.  Normal variants can hold these parameters as a single flat block
with multiple fields for each entry.  Polymorphic variants must adopt a more
flexible uniform memory representation since they may be re-used in a different
subtype elsewhere.  They allocate a tuple block for the parameters that is
pointed to from the argument field of the variant. Thus, there are three
additional words for such variants, along with an extra memory indirection due
to the tuple.

### The representation of strings

Strings are standard OCaml heap blocks, with the header size defining the size
of the string in machine words.  The actual block contents are the contents of
the string, and padding bytes to align the block on a word boundary.  On a
32-bit machine, the padding is calculated based on the modulo of the string
length and word size to ensure the result is word-aligned.

String length mod 4  Padding
-------------------  -------
0                    `00 00 00 03`
1                    `00 00 02`
2                    `00 01`
3                    `00`

Thus, the string contents are always zero-terminated by the padding word, and
its length can be computed quickly by:

~~~~~~~~~~~~~~~~~~~~~~~~~~~
number_of_words_in_block * sizeof(word) - last_byte_of_block - 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The guaranteed NULL-termination comes in handy when passing a string to C, but
is not relied upon to compute the length from OCaml. Thus, OCaml strings can
contain nulls at any point within the string.

### Custom heap blocks

OCaml supports *custom* heap blocks that have a special `Custom_tag` that let
the runtime perform user-defined operations over OCaml values.  A custom block
lives in the OCaml heap like an ordinary block and can be of whatever size the
user desires.  However, the runtime does not know anything about the structure
of the data in the block, other than that the first word of the custom block is
a C pointer to a `struct` of custom operations. The custom block cannot have
pointers to OCaml blocks and is considered opaque to the garbage collector.

~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The custom operations specify how the runtime should perform polymorphic
comparison, hashing and binary marshalling.  They also optionally contain a
finalizer, which the runtime will call just before the block is garbage
collected.  This finalizer has nothing to do with ordinary OCaml finalizers, as
created by `Gc.finalise`. (_avsm_: xref to GC module explanation)

When a custom block is allocated, you can also specify the proportion of
"extra-heap resources" consumed by the block, which will affect the garbage
collector's decision as to how much work to do in the next major slice.
(_avsm_: elaborate on this or move to the C interface section)


