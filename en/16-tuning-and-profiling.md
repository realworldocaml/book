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

Page table status      Value  Meaning
-----------------      -----  -------
`In_heap`              1      in the major heap
`In_young`             2      in the minor heap
`In_static_data`       4      in the statically allocated data segment
`In_code_area`         8      in the statically allocated code segment

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

As soon as the runtime finds a free block that is larger than the request,
there are three possibilities:

* If the free block is exactly the right size, it is unlinked from the free list and returned as the requested block.
* If the free block is one word too large, it is unlinked from the free list, and the
first word is given a special header recognizable to the collector as an unused word, while the rest of the block is returned as the requested block.
* If the free block is two or more words larger than the requested block, it remains in the free list, with its length shortened, and the end of the free block is returned for the requested block.  Since the allocated block is right-justified within the free block, the linking of the free list doesn't need to be changed at all as the block that remains in the free list is the original one.

### Memory allocation strategies

Allocating a new block in the major heap always looks in the free list.  There
are two allocation policies: first fit and next fit (the default).

####  Next-fit allocation

Next-fit allocation keeps a pointer to the block in the free list that was most
recently used to satisfy a request.  When a new request comes in, the allocator
searches from the next block until the end of the free list, and then from the beginning
of the free list up to that block.

#### First-fit allocation

First-fit allocation focusses on reducing memory fragmentation, at the expense of
slower block allocation. For some workloads, the reduction in the frequency in heap
compaction will outweigh the extra allocation cost. (_avsm_: example?)

The runtime maintains an ordered array of freelist chunks, called the `flp`
array.  Imagine a function mapping a block's index in the free list to its
size. The flp array pointers are to the high points of this graph. That is, if
you walk the free list between `flp[i]` and `flp[i+1]`, you will come across
blocks that have sizes at most the size of `flp[i]`.
Furthermore this sequence of smaller-than-flp[i] blocks
cannot be extended, which is equivalent to saying `size(flp[i+1]) > size(flp[i])`.

When allocating, we first check the flp-array. If `flp[i]` is not big enough
for our new block, then we may as well skip to `flp[i+1]`, because everything
in the free list before then will also be too small.

If there's nothing big enough in the `flp` array, we extend it by walking the
free list starting at the *last* pointer in the `flp`-array, say `flp[N]`.  We
extend the `flp` array along the way, so that at each block, if this block is
bigger than the current last thing in `flp` (which is equivalent to saying this
is the biggest block we've ever seen, since the blocks pointed to by the ~flp~
array are increasing in size), we add it to the end of ~flp~. We stop this walk
when we come across a block big enough to house our desired new block.

There's also the case when the `flp` array has its ceiling size of `FLP_MAX` (default
100). Then we just start at the end of the `flp` array and walk until we find something
big enough. This is known in the as a slow first-fit search, since this linear walk
may take a long time.

If we did manage to find something suitable in the `flp` array, say at index
`i`, we need to update `flp`. This update is rather complex, and the reason why
first-fit allocation is slower than next-fit. We walk through the free list
between `flp[i-1]` and `flp[i]` and record every high point we come across. Say
we find `j` such points. We move the upper portion of `flp` (from `flp[i+1]` to
the end) to the right by `j` places and insert each new high point into the
array. There is a further corner case when adding in `j` new high points would
make `flp` bigger than `FLP_MAX`.

(_avsm_: this really needs a diagram)

<note>
<title>Which allocation policy should I use?</title>

(_avsm_: 0 is the next-fit policy, which is quite fast but can result in fragmentation. 1 is the first-fit policy, which can be slower in some cases but can be better for programs with fragmentation problems. )

</note>

### Inter-generational pointers

Most incremental generational garbage collectors have to keep careful track of
values pointing from old generations to younger ones.  The OCaml runtime is no
exception, and maintains a set of addresses in the major heap that may point
into the minor heap.  These addresses are *not* OCaml pointers, and just
literal memory addresses.  The runtime ensures that it never relocates values
in the major heap unless this "remembered" set is empty.  The set is maintained
as a dynamically resized array of pointers, which is itself maintained via a
collection of pointers known as the `caml_ref_table`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .c }
struct caml_ref_table {
  value **base;
  value **end;
  value **threshold;
  value **ptr;
  value **limit;
  asize_t size;
  asize_t reserve;
};
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The relationships of the pointers are as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~
                   limit
   base <= ptr     threshold          end
     |----------------|----------------|
           size            reserve
~~~~~~~~~~~~~~~~~~~~~~~~~~~

An address is added to `caml_ref_table` when all of these conditions are satisfied:

* a field in a block in the major heap is mutated
* the field previously did not point to the minor heap
* the field is being changed to point into the minor heap 

In that case the entry is added at `caml_ref_table.ptr`, which is then
incremented.  If `ptr` is already at `limit`, the table is doubled in size
before adding the address.

The same address can occur in `caml_ref_table` multiple times if a block field
is mutated repeatedly and alternated between pointing at the minor heap and the
major heap.  The field in `caml_ref_table` also may not always point into the
minor heap (if it was changed after being added), since fields are never
removed. The entire table is cleared as part of the minor collection process.

#### The write barrier

The write barrier is one of the reasons why using immutable data structures can
sometimes be faster than mutable records.  The OCaml compiler keeps track of
any mutable types and adds a call to `caml_modify` before making the change.
The `caml_modify` checks that the remembered set is consistent, which, although
reasonably efficient, can be slower than simply allocating a fresh value on the
fast minor heap.

Let's see this for ourselves with a simple test program:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
type t1 = { mutable iters1: int; mutable count1: float }
type t2 = { iters2: int; count2: float }

let rec test_mutable t1 =
  match t1.iters1 with
  |0 -> ()
  |n ->
    t1.iters1 <- t1.iters1 - 1;
    t1.count1 <- t1.count1 +. 1.0;
    test_mutable t1

let rec test_immutable t2 =
  match t2.iters2 with
  |0 -> ()
  |n ->
    let iters2 = n - 1 in
    let count2 = t2.count2 +. 1.0 in
    test_immutable { iters2; count2 }

open Printf
let time name fn arg =
  Gc.compact ();
  let w1 = Gc.((stat ()).minor_collections) in
  let t1 = Unix.gettimeofday () in
  fn arg;
  let w2 = Gc.((stat ()).minor_collections) in
  let t2 = Unix.gettimeofday () in
  printf "%s: %.4fs (%d minor collections)\n" name (t2 -. t1) (w2 - w1)

let _ =
  let iters = 1000000000 in
  time "mutable" test_mutable { iters1=iters; count1=0.0 };
  time "immutable" test_immutable { iters2=iters; count2=0.0 }
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This program defines a type `t1` that is mutable, and `t2` that is immutable.
The main loop iterates over both fields and runs a simple counter.  It measures
two things: the wallclock time that all the iterations take, and the number of
minor garbage collections that occurred during the test.  The results should
look something like this:

~~~~~~~~~~~~~~~~~~~~~~~~~~~
mutable: 8.6923s (7629 minor collections)
immutable: 2.6186s (19073 minor collections)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Notice the space/time tradeoff here. The mutable version runs almost 4 times
slower than the immutable one, but has significantly fewer garbage collection
cycles.  Minor collections in OCaml are very fast, and so it is often
acceptable to use immutable data structures in preference to the more
conventional mutable versions.  On the other hand, if you only rarely mutable a
value, it can be faster to take the write barrier hit and not allocate at all.

(_avsm_: it would be really nice to use a benchmark suite here and shorten the
example. Investigate the options and edit this section)

(_avsm_: need to mention when a value is allocated directly into the major heap
somewhere)

## Byte code Profiling

ocamlcp and call trace information

## Native Code Profiling

### gdb

requires shinwell's patch in ocaml trunk via opam

### perf

requires fabrice's frame pointer patch

### dtrace

requires my dtrace/instruments patch for libasmrun
