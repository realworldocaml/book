# Understanding the runtime system

Much of the static type information contained within an OCaml program is
checked and discarded at compilation time, leaving a much simpler *runtime*
representation for values.  Understanding this runtime layout is important to
writing fast programs, and interfacing with C libraries which work directly
with these values.

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
programs.  The runtime system manages a "heap", which a collection of memory
regions it obtains from the operating system using `malloc`. The OCaml runtime
uses these memory regions to hold "heap blocks", which it then fills up in
response to allocation requests by the OCaml program.

When there is'nt enough memory available to satisfy an allocation request from
the allocated heap blocks, the runtime system invokes the "garbage collector"
(or GC). An OCaml program does not explicitly free a heap block when it is done
with it, and the GC must determine which heap blocks are "alive" and which heap
blocks are "dead", i.e. no longer in use.  Dead blocks are collected and their
memory made available for re-use by the application.

The garbage collector does not keep constant track of blocks as they are
allocated and used.  Instead, it regularly scans blocks by starting from a set
of "roots", which are values that the application always has access to (such as
the stack).  Thus, the GC maintains a directed graph in which heap blocks are
nodes, and there is an edge from heap block `b1` to heap block `b2` if some
field of `b1` points to `b2`.  All blocks reachable from the roots by following
edges in the graph must be retained, and unreachable blocks can be reused.

With the typical OCaml programming style, many small blocks are frequently
allocated, used for a short period of time, and then never used again.  OCaml
takes advantage of this fact to improve the performance of allocation and
collection by using a "generational" garbage collector, which means that it has
different memory regions to hold blocks based on how long the blocks have been
alive.  Specifically, OCaml's heap is split in two; there is a small,
fixed-size "minor heap" (or "young generation") used for initially allocating
most blocks, and a large, variable-sized "major heap" (or "old generation") for
holding blocks that have been alive longer or are larger than 4KB.  A typical
functional programming style means that young blocks tend to die young, and old
blocks tend to stay around for longer than young ones (this is referred to as
the "generational hypothesis"). To reflect this, OCaml uses different memory
layouts and garbage collection algorithms for the major and minor heaps.

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

The major heap consists of a number of chunks of memory, each containing live
blocks interspersed with regions of free memory.  The runtime system maintains
a "free list", which is a data structure indexing all the free memory, that is
used to satisfy allocation requests.  OCaml uses *mark-sweep-compact* garbage
collection for the major heap.  The "mark" phase to traverses the block graph
and marks all live blocks, literally by setting a bit in the block header.  The
"sweep" phase sequentially scans all heap memory and identifies dead block.
The "compact" phase happens more rarely, and moves live blocks to eliminate the
gaps of free memory between them, and ensure memory does not fragment.

A garbage collection must "stop the world" (that is, halt the application) in
order to ensure that memory can be safely moved. To minimise this impact, the
mark and sweep phases are incremental, and are broken up into a number of steps
that are interspersed with the running OCaml program.  Only a compaction
touches all the memory in one go, and it is a relatively rare operation.
We will discuss garbage collection tuning in (_avsm_: crossref).

## The representation of values

Every OCaml *value* is a single word that is either an integer or a pointer
elsewhere in memory.  If the lowest bit of the word is non-zero, the value is
an unboxed integer.  Several OCaml types map onto integers, including `bool`,
`int`, the empty list, `unit`, and variants without constructors.  Integers are
the only unboxed runtime values in OCaml, and are thus the cheapest values to
allocate and manipulate.  A `value` containing a pointer is stored unmodified
since pointers are guaranteed to be word-aligned, so the bottom bits are
zeroed. If the pointer is inside an area managed by the runtime, it is assumed
to point to a valid OCaml *block*, or else is treated as an opaque C pointer to
some other system resource.

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

A *block* is the basic unit of allocation on the heap.  A block consists of a
one-word header (either 32- or 64-bits) followed by variable-length data, which
is either opaque bytes or "fields", which are OCaml values.  The collector
never inspects opaque bytes, which hence should never contain OCaml pointers.
The runtime always inspects fields, and follows them as part of the garbage
collection process described earlier.  Every block header has a tag that
defines its runtime type, and how to interprete the subsequent fields.

(_avsm_: pointers to blocks actually point 4/8 bytes into it, for some efficiency
reason that I cannot recall right now).

~~~~~~~~~~~~~~~~~~~~~~~~~~~
(TODO draw this properly)
+------------------------+-------+----------+----------+----------+----
| size of block in words |  col  | tag byte | value[0] | value[1] | ...
+------------------------+-------+----------+----------+----------+----
 <-either 22 or 54 bits-> <2 bit> <--8 bit-->
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The size part records the length of the block in words. Note that it is limited
to 22-bits on 32-bit platforms, which is the reason why OCaml strings are
limited to 16MB.  If you need bigger strings, then either switch to a 64-bit
host, or use the `Bigarray` module.  The 2-bit color is used by the garbage
collector to keep track of the mark and sweep status, and is not exposed directly
to OCaml programs.

Tag Color   Block Status
---------   ------------
blue        on the free list and not currently in use
white       not reached yet, but possibly reachable
gray        reachable, but its fields have not been scanned
black       reachable, and its fields have been scanned

A block's tag byte indicates whether the data array represents opaque bytes or
fields.  Tags are also used to distinguish constructors of an OCaml variant
type.  If a block's tag is greater than or equal to `No_scan_tag` (251), then
the block's data is opaque bytes, and are not scanned by the collector. The
most common such block is the `string` type.

(_avsm_: too much info here) If the header is zero, then the object has been
forwarded as part of minor collection, and the first field points to the new
location.  Also, if the block is on the ~oldify_todo_list~, part of the minor
gc, then the second field points to the next entry on the oldify_todo_list.

### The representation of strings

Strings are standard OCaml heap blocks, with the header size defining the size
of the string in machine words.  The actual block contents are the contents of
the string, and padding bytes to align the block on a word boundary.  On a
32-bit machine, the padding is one of

~~~~~~~~~~~~~~~~~~~~~~~~~~~
 : 00
 : 00 01
 : 00 00 02
 : 00 00 00 03
for 64-bit:
 : 00 00 00 00 04
 : 00 00 00 00 00 05
 : 00 00 00 00 00 00 06
 : 00 00 00 00 00 00 00 07
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Thus, the string contents are always zero-terminated by the padding word, and
its length can be computed very quickly by:

~~~~~~~~~~~~~~~~~~~~~~~~~~~
number_of_words_in_block * sizeof(word) + last_byte_of_block - 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The guaranteed NULL-termination comes in handy when passing a string to C, but
is not relied upon to compute the length from OCaml. Thus, OCaml strings can
contain nulls at any point within the string.

### Custom heap blocks

OCaml supports "custom" heap blocks that have a special tag, ~Custom_tag~, and
are treated specially by the runtime.  A custom block lives in the OCaml heap
like an ordinary block and can be of whatever size the user desires.  However,
the runtime does not know anything about the structure of the data in the
block, other than that the first word of the custom block is a C pointer to a
`struct` of custom operations. The custom block cannot have pointers to OCaml
blocks.

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

## Bigarrays for external memory blocks

An OCaml bigarray is a useful custom block provided as standard to manipulate
memory blocks outside the OCaml heap.  It has `Custom_tag` in the header, and
the first word points to the `custom_operations` struct for bigarrays.
Following this is a `caml_ba_array` struct.

~~~~~~~~~~~~~~~~~~~~~~~~~~~
struct caml_ba_array {
  void * data;                  /* Pointer to raw data */
  intnat num_dims;              /* Number of dimensions */
  intnat flags;                 /* Kind of element array + memory layout + allocation status */
  struct caml_ba_proxy * proxy; /* The proxy for sub-arrays, or NULL */
  intnat dim[]  /*[num_dims]*/; /* Size in each dimension */
};
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `data` is usually a pointer to a `malloc`'ed chunk of memory, which the
custom finalizer operation `free`'s when the block is free.  The `flags` field
encodes three values, located in the bits as specified by three masks:

~~~~~~~~~~~~~~~~~~~~~~~~~~~
CAML_BA_KIND_MASK = 0xFF     /* Mask for kind in flags field */
CAML_BA_LAYOUT_MASK = 0x100  /* Mask for layout in flags field */
CAML_BA_MANAGED_MASK = 0x600 /* Mask for "managed" bits in flags field */
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `CAML_BA_KIND_MASK` bits hold a value of the `caml_ba_kind` enum that identifies the
kind of value in the bigarray `data`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~
enum caml_ba_kind {
  CAML_BA_FLOAT32,             /* Single-precision floats */
  CAML_BA_FLOAT64,             /* Double-precision floats */
  CAML_BA_SINT8,               /* Signed 8-bit integers */
  CAML_BA_UINT8,               /* Unsigned 8-bit integers */
  CAML_BA_SINT16,              /* Signed 16-bit integers */
  CAML_BA_UINT16,              /* Unsigned 16-bit integers */
  CAML_BA_INT32,               /* Signed 32-bit integers */
  CAML_BA_INT64,               /* Signed 64-bit integers */
  CAML_BA_CAML_INT,            /* OCaml-style integers (signed 31 or 63 bits) */
  CAML_BA_NATIVE_INT,          /* Platform-native long integers (32 or 64 bits) */
  CAML_BA_COMPLEX32,           /* Single-precision complex */
  CAML_BA_COMPLEX64,           /* Double-precision complex */
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `CAML_BA_LAYOUT_MASK` bit says whether multi-dimensional arrays are layed out C or
Fortran style.

~~~~~~~~~~~~~~~~~~~~~~~~~~~
enum caml_ba_layout {
  CAML_BA_C_LAYOUT = 0,           /* Row major, indices start at 0 */
  CAML_BA_FORTRAN_LAYOUT = 0x100, /* Column major, indices start at 1 */
};
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `CAML_BA_MANAGED_MASK` bits hold a value of the `caml_ba_managed` enum that identifies
whether OCaml is responsible for freeing the `data` or some other code is.

~~~~~~~~~~~~~~~~~~~~~~~~~~~
enum caml_ba_managed {
  CAML_BA_EXTERNAL = 0,        /* Data is not allocated by OCaml */
  CAML_BA_MANAGED = 0x200,     /* Data is allocated by OCaml */
  CAML_BA_MAPPED_FILE = 0x400, /* Data is a memory mapped file */
};
~~~~~~~~~~~~~~~~~~~~~~~~~~~

