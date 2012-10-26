# Tuning and Profiling

## Tuning the Garbage Collector

### How the runtime tracks memory usage

The OCaml runtime divides the address space into memory pages of 4KB each (this
is configurable by recompiling the runtime).  At any given time, every page
that is in use is used for a single purpose: major heap, minor heap, static
data or code.  The runtime guarantees this by always allocating slightly more
memory than requested so that that it can choose align the memory it will
actually use at the beginning of a 4KB page.

The runtime maintains a *page table* that allows it to determine the status of
any virtual memory address in the operating system process.  The status defines
whether that address is a page in use by the OCaml runtime, and if so, which of
the four purposes it is being used for.

Since the virtual memory space can be very large and sparsely used (especially
on a 64-bit CPU), the page table is implemented as a hash table in which keys
are page-aligned addresses and values are a single byte. The hash table is
represented as an array of words, with each word being a key-value pair.  The
key-value pair is the bitwise `or` of the virtual address of the start of the
page (which has zeros for its lower 12-bits due to being aligned to 4KB), and
the lower 8 bits are used for the value.  To look up an address, one masks out
the lower 12-bits of the memory address, compute a multiplicative hash to get a
table index, and then compares against the address (i.e. the key) at that
index.  Linear probing is used to resolve collisions.

The byte value stored is a bitwise `or` of the following status bits:

Page table status      Value
-----------------      -----
`In_heap`              1
`In_young`             2
`In_static_data`       4
`In_code_area`         8

The page table starts with a size aiming to be between 25% and 50% full of entries, and is
automatically doubled in size if it becomes half full.  It is never shrunk.

### Allocating on the minor heap

The minor heap is a contiguous chunk of virtual memory.  Its size is set on
program startup and decided by the `OCAMLRUNPARAM` environment variable
(_avsm_: xref), and then only changed later by calls to `Gc.set`.  The default
size is 256k.

The range of memory usable for allocation goes from the `caml_young_start` to
`caml_young_end` C variables managed by the runtime.

~~~~~~~~~~~~~~~~~~~~~~~~~~~
                <---- size ---->
 base --- start ---------------- end
          limit      ptr <------
                          blocks
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In a fresh minor heap, the `limit` will equal the `start`, and the current
`ptr` will equal the `end`.  As blocks are allocated, `caml_young_ptr` will
decrease until it reaches `caml_young_limit`, at which point a minor garbage
collection is triggered.  To allocate a block in the minor heap, we decrement
`caml_young_ptr` by the size of the block (including the header), and then set
the the header to a valid value.  If there isn't enough space left for the
block without decrementing past the `limit`, a minor collection is triggered.

To force a minor gc to occur, one can set the `caml_young_limit` to equal `caml_young_end`, which
causes signal handlers to be run and to "urge" the runtime
(_avsm_: elaborate on this urging business, and how to set young from within OCaml via `Gc.??`).

### Managing the major heap

The major heap is a singly linked list of contiguous memory chunks, sorted in
increasing order of virtual address.  Each chunk is a single memory chunk
allocated via *malloc(3)* and consists of a header and a data area which
contains OCaml blocks.  A pointer to a heap chunk points to the start of the
data area, and access to the header is done by a negative offset from this
pointer.  A chunk header has:

* the address of the memory that the chunk is in, as allocated by *malloc(3)*. It is needed when the chunk is freed.
* the size in bytes of the data area
* an allocation size in bytes, used during heap compaction to merge small blocks to defragment the heap.
* a link to the next heap chunk in the list.

The chunk's data area always starts on a page boundary, and its size is a
multiple of the page size (4KB).  It contains a contiguous sequence of heap
blocks. These can be as small as one or two 4KB pages, but are usually
allocated in 1MB chunks (or 512KB on 32-bit architectures).  You can modify
these defaults by editing `Heap_chunk_def` in `byterun/config.h` and
recompiling the runtime. (_avsm_: talk about modifying the defaults in a
separate callout, as there are quite a few variables which can be tweaked)

Allocating a block on the major heap first checks the free list of blocks (see
below).  If there isn't enough room on the free list, the runtime expands the
major heap with a fresh block that will be large enough.  That block is then
added to the free list, and the free list is checked again (and this time will
definitely succeed).

#### The major heap free list

The free space in the major heap's chunks is organized as a singly linked list
of OCaml blocks, ordered by increasing virtual address.  The runtime has a
pointer to the first block in the free list.  A free list block is at least two
words: a header followed by a pointer to the next free-list block.  The header
specifies the length of the block just as with a normal block.
(_avsm_: I'm not sure that this is quite true. It seems from `freelist.c` that
the freelist blocks are normal OCaml blocks, with the first data entry being
the next pointer. when detached, they become normal ocaml blocks)

#### Allocation policies

Allocating a new block in the major heap always looks in the free list.  There
are two allocation policies: first fit and next fit (the default).

As soon as the runtime finds a free block that is larger than the request,
there are three possibilities.  If the free block is exactly the right size, it
is unlinked from the free list and returned as the requested block.  If the
free block is one word too large, it is unlinked from the free list, and the
first word is given a special header recognizable to the collector as an unused
word, while the rest of the block is returned as the requested block.  Finally,
if the free block is two or more words larger than the requested block, it
remains in the free list, with its length shortened, and the end of the free
block is used for the requested block.


## Byte code Profiling

ocamlcp and call trace information

## Native Code Profiling

### gdb

requires shinwell's patch in ocaml trunk via opam

### perf

requires fabrice's frame pointer patch

### dtrace

requires my dtrace/instruments patch for libasmrun
