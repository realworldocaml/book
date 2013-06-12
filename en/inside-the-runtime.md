# Understanding the Garbage Collector

We've described the runtime format of individual OCaml variables earlier in
[xref](#memory-representation-of-values).  When you execute your program, OCaml
manages the lifecycle of these variables by regularly scanning allocated values
and freeing them when they're no longer needed.  This in turn means that your
applications don't need to manually implement memory management and greatly
reduces the likelihood of memory leaks creeping into your code.

The OCaml runtime is a C library that provides routines that can be called from
running OCaml programs.  The runtime manages a *heap*, which is a collection of
memory regions that it obtains from the operating system. The runtime uses this
memory to hold *heap blocks* that it fills up with OCaml values in response to
allocation requests by the OCaml program.

## Mark and sweep garbage collection

When there isn't enough memory available to satisfy an allocation request from
the pool of allocated heap blocks, the runtime system invokes the *garbage
collector* (or GC). An OCaml program can't explicitly free a value when it is
done with it. Instead, the GC regularly determines which values are *live* and
which values are *dead*, i.e. no longer in use. Dead values are collected and
their memory made available for reuse by the application.

The garbage collector doesn't keep constant track of values as they are
allocated and used. Instead, it regularly scans them by starting from a set of
*root* values that the application always has access to (such as the stack).
The GC maintains a directed graph in which heap blocks are nodes, and there is
an edge from heap block `b1` to heap block `b2` if some field of `b1` points to
`b2`.  All blocks reachable from the roots by following edges in the graph must
be retained, and unreachable blocks can be reused by the application.  This
strategy is commonly known as *mark and sweep* garbage collection.

## Generational garbage collection

The usual OCaml programming style involves allocating many small variables that
are used for a short period of time and then never accessed again. OCaml takes
advantage of this fact to improve performance by using a *generational* garbage
collector.

A generational GC maintains separate memory regions to hold blocks based on how
long the blocks have been live.  OCaml's heap is split in two such regions:

* a small fixed-size *minor heap* where most blocks are initially allocated.
* a larger variable-sized *major heap* for blocks that have been live longer.

A typical functional programming style means that young blocks tend to die
young and old blocks tend to stay around for longer than young ones.  This is
often referred to as the *generational hypothesis*. 

OCaml uses different memory layouts and garbage collection algorithms for the
major and minor heaps to account for this generational difference.  We'll
explain how they differ in more detail next.

<sidebar>
<title>The `Gc` module and `OCAMLRUNPARAM`</title>

OCaml provides several mechanisms to query and alter the behaviour of the
runtime system.  The `Gc` module provides this functionality from within OCaml
code, and we'll frequently refer to it in the rest of the chapter.  As with
several other standard library modules, Core alters the `Gc` interface from the
standard OCaml library.  We'll assume that you've opened `Core.Std` in our
explanations.

You can also control the behaviour of OCaml programs by setting the
`OCAMLRUNPARAM` environment variable before launching your application.  This
lets you set garbage collector parameters without recompiling, for example to
benchmark the effects of different settings.  The format of `OCAMLRUNPARAM` is
documented in the [OCaml manual](http://caml.inria.fr/pub/docs/manual-ocaml/manual024.html).

</sidebar>

## The fast minor heap

The minor heap is where most of your short-lived values are held.  It consists
of one contiguous chunk of virtual memory containing a sequence of OCaml
blocks.  If there is space, allocating a new block is a fast constant-time
operation that requires just a couple of CPU instructions.

To garbage collect the minor heap, OCaml uses *copying collection* to move all
live blocks in the minor heap to the major heap.  This takes work proportional
to the number of live blocks in the minor heap, which is typically small
according to the generational hypothesis.

### Allocating on the minor heap

The minor heap is a contiguous chunk of virtual memory that is usually a few
megabytes in size so that it can be scanned quickly.  The runtime stores the
minor heap in two pointers (`caml_young_start` and `caml_young_end`) that
delimit the start and end of the heap region.

```
                <---- size ---->
 base --- start ---------------- end
          limit      ptr <------
                          blocks
```

In a fresh minor heap, the `limit` equals the `start` and the current `ptr`
will equal the `end`.  `ptr` decreases as blocks are allocated until it reaches
`limit`, at which point a minor garbage collection is triggered.  To allocate a
block in the minor heap, `ptr` is decremented by the size of the block
(including the header) and the header area is immediately set to a valid value.
If there isn't enough space left for the block without decrementing past the
`limit`, a minor garbage collection is triggered.

You may wonder why `limit` is required at all, since it always seems to equal
`start`.  It's because the easiest way for the runtime to schedule a minor heap
collection is by setting `limit` to equal `end`.  The next allocation will
never have enough space after this is done and will always trigger a garbage
collection.

<note>
<title>Setting the size of the minor heap</title>

The minor heap size defaults to 8MB on 64-bit platforms, unless overridden by
the `s=<words>` argument to `OCAMLRUNPARAM`.  You can change it after the
program has started by calling the `Gc.set` function.

```ocaml
# open Gc;;
# let c = Gc.get ();;    
val c : Gc.control =
  {minor_heap_size = 262144; major_heap_increment = 126976;
   space_overhead = 80; verbose = 0; max_overhead = 500;
   stack_limit = 1048576; allocation_policy = 0}
# Gc.tune ~minor_heap_size:(262144 * 2) () ;;
- : unit = ()
```

Changing the GC size dynamically will trigger an immediate minor heap
collection.  Note that Core increases the default minor heap size from the
standard OCaml installation quite significantly, and you'll want to reduce this
if running in very memory-constrained environments.

</note>

## The long-lived major heap

The major heap is where the bulk of the longer-lived and larger values in your
program are stored.  It consists of any number of non-contiguous chunks of
virtual memory, each containing live blocks interspersed with regions of free
memory.  The runtime system maintains a free-list data structure that indexes
all the free memory that it has allocated, and uses it to satisfy allocation
requests for OCaml blocks.

The major heap is typically much larger than the minor heap and can scale to
gigabytes in size. It is cleaned via a mark-and-sweep garbage collection
algorithm that operates in several phases:

* The *mark* phase scans the block graph and marks all live blocks by setting
  a bit in the tag of the block header (known as the *color* tag).
* The *sweep* phase sequentially scans the heap chunks and identifies dead blocks 
  that weren't marked earlier.
* The *compact* phase relocates live blocks into a freshly allocated heap to 
  eliminate gaps in the free list. This prevents the fragmentation of heap blocks
  in long-running programs.

A major garbage collection must *stop the world* (that is, halt the
application) to ensure that blocks can be moved around without this being
observed by the live application. The mark-and-sweep phases run incrementally
over slices of the heap to avoid pausing the application for long periods of
time.  Only the compaction phase touches all the memory in one go, and is a
relatively rare operation.

### Allocating on the major heap

The major heap consists of a singly-linked list of contiguous memory chunks
sorted in increasing order of virtual address.  Each chunk is a single memory
region allocated via *malloc(3)* and consists of a header and data area which
contains OCaml heap chunks.  A heap chunk header contains:

* the *malloc*'ed virtual address of the memory region containing the hunk.
* the size in bytes of the data area.
* an allocation size in bytes used during heap compaction to merge small blocks to defragment the heap.
* a link to the next heap chunk in the list.

Each chunk's data area starts on a page boundary and its size is a multiple of
the page size (4KB).  It contains a contiguous sequence of heap blocks which
can be as small as one or two 4KB pages, but are usually allocated in 1MB
chunks (or 512KB on 32-bit architectures).

<note>
<title>Controlling major heap growth</title>

The `Gc` module uses the `major_heap_increment` value to control the major heap
growth.  This defines the number of words to add to the major heap per
expansion, and is the only memory allocation operation that the operating
system observes from the OCaml runtime after initial startup (since the minor
is fixed in size).  

If you anticipate allocating some large OCaml values, then setting the heap
increment to a larger value will let the operating system return a contiguous
block of memory.  This is preferable to lots of smaller heap chunks that may be
spread across different regions of virtual memory, and require more
housekeeping in the OCaml runtime to keep track of them.

```ocaml
# open Core.Std;;
# Gc.tune ~major_heap_increment:(1000448 * 4) ();;
```

</note>

Allocating an OCaml value on the major heap first checks the free list of
blocks for a suitable region to place it.  If there isn't enough room on the
free list, the runtime expands the major heap by allocating a fresh heap chunk
that will be large enough.  That chunk is then added to the free list and the
free list is checked again (and this time will definitely succeed).

Remember that most allocations to the major heap will go via the minor heap,
and only be promoted if they are still used by the program after a minor
collection.  The one exception to this is for values larger than 256 words
(that is, 2kB on 64-bit platforms).  These will be allocated directly on the
major heap since an allocation on the minor heap would likely trigger an
immediate collection and copy it to the major heap anyway.

### Memory allocation strategies

The major heap does its best to manage memory allocation as efficiently as possible,
and relies on heap compaction ot ensure that memory stays contiguous and unfragmented.
The default allocation policy normally works fine for most applications, but
it's worth bearing in mind that there are other options too.

The free list of blocks is always checked first when allocating a new block in
the major heap.  The default free list search is called *next-fit allocation*,
with an alternative *first-fit* algorithm also available.

####  Next-fit allocation

Next-fit allocation keeps a pointer to the block in the free list that was most
recently used to satisfy a request.  When a new request comes in, the allocator
searches from the next block until the end of the free list, and then from the beginning
of the free list up to that block.

Next-fit allocation is the default allocation strategy.  It's quite a cheap
allocation mechanism since the same heap chunk can be reused across allocation
requests until it runs out.  This in turn means that there is good memory
locality to use CPU caches better.

#### First-fit allocation

If your programs allocates values of many varied sizes, you may sometimes find
that your free list becomes fragmented.  In this situation, the GC is forced to
perform an expensive compaction despite there being free chunks, since none of
the chunks alone are big enough to satisfy the request.

First-fit allocation focusses on reducing memory fragmentation, but at the
expense of slower block allocation.  Every allocation scans the free list from
the beginning for a suitable free chunk, instead of reusing the most recent
heap chunk as the next-fit allocator does.

For some workloads, the reduction in the frequency in heap compaction will
outweigh the extra allocation cost.

<note>
<title>Controlling the heap allocation policy</title>

You can set the heap allocation policy via the `Gc.allocation_policy` field.
A value of `0` (the default) sets it to next-fit, and `1` to the first-fit
allocator.

The same behaviour can be controlled at runtime by setting `a=0` or `a=1`
in `OCAMLRUNPARAM`.

</note>

### Marking and scanning the heap

The marking process can take a long time to run over the complete major heap,
and has to pause the main application while it's active.  It therefore runs
incrementally by marking the heap in *slices*.  Each value in the heap has a
2-bit *color* field in its header that is used to store information about
whether the value has been marked, so that the GC can resume easily between
slices.

Tag Color   Block Status
---------   ------------
blue        on the free list and not currently in use
white       not reached yet, but possibly reachable
gray        reachable, but its fields have not been scanned
black       reachable, and its fields have been scanned


The marking process starts with a set of *root* values that are always live
(such as the application stack).  All values on the heap are initially marked
as white values that are possibly reachable, but haven't been scanned yet.  It
recursively follows all the fields in the roots via a depth-first search, and
pushes newly encountered white blocks onto an intermediate stack of *gray
values* while it follows their fields.  When a gray value's fields have all
been followed it is popped off the stack and colored black.

This process is repeated until the gray value stack is empty and there are no
further values to mark.  There's one important edge case in this process,
though.  The gray value stack can only grow to a certain size, after which the
GC can no longer recurse into intermediate values since it has nowhere to store
them while it follows their fields.  If this happens, the heap is marked as
*impure* and a more expensive check is initiated once the existing gray values
have been processed.

To mark an impure heap, the GC first marks it as pure and walks through the
entire heap block-by-block in increasing order of memory address. If it finds a
gray block, it adds it to the gray list and recursively marks it using the
usual strategy for a pure heap.  Once the scan of the complete heap is
finished, the mark phase checks again whether the heap has again become impure,
and repeats the scan until if it is. These full-heap scans will continue until
a successful scan completes without overflowing the gray list.

<note>
<title>Controlling major heap collections</title>

You can trigger a single slice of the major GC via the `major_slice` call.
This performs a minor collection first, and then a single slice.  The size of
the slice is normally automatically computed by the GC to an appropriate value,
and returns this value so that you can modify it in future calls if necessary.

```ocaml
# open Core.Std;;
# Gc.major_slice 0 ;;
- : int = 232340
# Gc.full_major ();;
- : unit = ()
```

The `space_overhead` setting controls how aggressive the GC is about setting
the slice size to a large size.  This represents the proportion of memory used
for live data that will be "wasted" because the GC doesn't immediately collect
unreachable blocks.  Core defaults this to `100` to reflect a typical system
that isn't overly memory-constrained. Set this even higher if you have lots of
memory, or lower to cause the GC to work harder and collect blocks faster at
the expense of using more CPU time.

</note>

### Inter-generational pointers

One complexity of generational collection arises from the fact that minor heap
sweeps are much more frequent than major heap collections. In order to know
which blocks in the minor heap are live, the collector must track which
minor-heap blocks are directly pointed to by major-heap blocks.  Without this
information, each minor collection would also require scanning the much larger
major heap.

OCaml maintains a set of such *inter-generational pointers* to avoid this
dependency between a major and minor heap collection.  The compiler introduces
a write barrier to update this so-called *remembered set* whenever a major-heap
block is modified to point at a minor-heap block.

#### The mutable write barrier

The write barrier can have profound implications for the structure of your
code.  It's one of the reasons why using immutable data structures and
allocating a fresh copy with changes can sometimes be faster than mutating a
record in-place.

The OCaml compiler keeps track of any mutable types and adds a call to the
runtime `caml_modify` function before making the change.  This checks the
location of target write and the value its being changed to, and ensures that
the remembered set is consistent.  Although the write barrier is reasonably
efficient, it can sometimes be slower than simply allocating a fresh value on
the fast minor heap and doing some extra minor collections.

Let's see this for ourselves with a simple test program.  You'll need
to install the Core benchmarking suite via `opam install core_bench` before
you compile this code.

```ocaml
(* barrier_bench.ml: benchmark mutable vs immutable writes *)
open Core.Std
open Core_bench.Std

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

let () =
  let iters = 1000000 in
  let tests = [
    Bench.Test.create ~name:"mutable" 
      (fun () -> test_mutable { iters1=iters; count1=0.0 });
    Bench.Test.create ~name:"immutable"
      (fun () -> test_immutable { iters2=iters; count2=0.0 })
  ] in
  Bench.make_command tests |> Command.run

```

This program defines a type `t1` that is mutable and `t2` that is immutable.
The benchmark loop iterates over both fields and increments a counter.
Compile and execute this with some extra options to show the amount of
garbage collection occurring.

```console
$ ocamlbuild -use-ocamlfind -package core -package core_bench -tag thread barrier_bench.native
$ ./barrier_bench.native name allocated
Estimated testing time 20s (change using -quota SECS).
┌───────────┬───────────┬─────────────────────┬───────────┬────────┬──────────┬────────────┐
│ Name      │ Time (ns) │           Time 95ci │     Minor │  Major │ Promoted │ Percentage │
├───────────┼───────────┼─────────────────────┼───────────┼────────┼──────────┼────────────┤
│ mutable   │ 7_954_262 │ 7_827_275-8_135_261 │ 2_000_004 │ -51.42 │   -51.42 │     100.00 │
│ immutable │ 3_694_618 │ 3_396_611-4_037_053 │ 5_000_005 │ -28.43 │   -28.43 │      46.45 │
└───────────┴───────────┴─────────────────────┴───────────┴────────┴──────────┴────────────┘
```

There is a stark space/time tradeoff here. The mutable version takes
significantly longer to complete than the immutable one, but allocates many
fewer minor heap words than the immutable version.  Minor allocation in OCaml
is very fast and so it is often better to use immutable data structures in
preference to the more conventional mutable versions.  On the other hand, if
you only rarely mutate a value, it can be faster to take the write barrier hit
and not allocate at all.

The only way to know for sure is to benchmark your program under real-world
scenarios using `Core_bench`, and experiment with the tradeoffs.  The
command-line benchmark binaries have a number of useful options that affect
garbage collection behaviour.

```
Benchmark for mutable, immutable

  barrier_bench.native [COLUMN ...]

Columns that can be specified are:
	name       - Name of the test.
	cycles     - Number of CPU cycles (RDTSC) taken.
	cycles95ci - 95% confidence interval and error for cycles.
	~cycles    - Cycles taken excluding major GC costs.
	time       - Number of nano secs taken.
	time95ci   - 95% confidence interval and error for time (ns).
	~time      - Time (ns) taken excluding major GC costs.
	allocated  - Allocation of major, minor and promoted words.
	percentage - Relative execution time as a percentage.
	gc         - Show major and minor collections.
	speedup    - Relative execution cost as a speedup.
	samples    - Number of samples collected for profiling.

The following columns will be displayed by default:
	+name time time95ci percentage

To specify that a column should be displayed only if it has a non-trivial value,
prefix the column name with a '+'.

=== flags ===

  [-clear-columns]     Don't display default columns. Only show user specified
                       ones.
  [-display STYLE]     Table style (short, tall, line or blank). Default short.
  [-geometric SCALE]   Use geometric sampling. (default 1.01)
  [-linear INCREMENT]  Use linear sampling to explore number of runs, example 1.
  [-no-compactions]    Disable GC compactions.
  [-quota SECS]        Time quota allowed per test (default 10s).
  [-save]              Save benchmark data to <test name>.txt files.
  [-stabilize-gc]      Stabilize GC between each sample capture.
  [-v]                 High verbosity level.
  [-width WIDTH]       width limit on column display (default 150).
  [-build-info]        print info about this build and exit
  [-version]           print the version of this build and exit
  [-help]              print this help text and exit
                       (alias: -?)

```

The `-no-compactions` and `-stabilize-gc` options can help force a situation
where your application has fragmented memory.  This can simulate the behaviour
of a long-running application without you having to actually wait that long to
recreate the behaviour in a performance unit test.

## Attaching finalizer functions to values

OCaml's automatic memory management guarantees that a value will eventually be
freed when it's no longer in use, either via the garbage collector sweeping it
or the program terminating.  It's sometimes useful to run extra code just
before a value is freed by the garbage collector, for example to check that a
file descriptor has been closed, or that a log message is recorded.

<note>
<title>What values can be finalized?</title>

Various values cannot have finalizers attached since they aren't
heap-allocated.  Some examples of values that are not heap-allocated are
integers, constant constructors, booleans, the empty array, the empty list and
the unit value. The exact list of what is heap-allocated or not is
implementation-dependent, which is why Core provides the `Heap_block` module to
explicitly check before attaching the finalizer.

Some constant values can be heap-allocated but never deallocated during the
lifetime of the program, for example a list of integer constants.  `Heap_block`
explicitly checks to see if the value is in the major or minor heap, and
rejects most constant values.  Compiler optimisations may also duplicate some
immutable values such as floating-point values in arrays. These may be
finalised while another duplicate copy is being used by the program.

For this reason, attach finalizers only to values that you are explicitly sure
are heap-allocated and aren't immutable.  A common use is to attach them to
file descriptors to ensure it is closed.  However, the finalizer normally
shouldn't be the primary way of closing the file descriptor, since it depends
on the garbage collector running in order to collect the value.  For a busy
system, you can easily run out of a scarce resource such as file descriptors
before the GC catches up.

</note>

Core provides a `Heap_block` module that dynamically checks if a given value is
suitable for finalizing.  This block is then passed to Async's
`Gc.add_finalizer` function that schedules the finalizer safely with respect to
all the other concurrent program threads.

Let's explore this with a small example that finalizes values of different
types, some of which are heap-allocated and others which are compile-time
constants.

```ocaml
(* finalizer.ml : explore finalizers for different types *)
open Core.Std
open Async.Std

let attach_finalizer n v =
  match Heap_block.create v with
  | None -> printf "%20s: FAIL\n%!" n
  | Some hb ->
      let final _ = printf "%20s: OK\n%!" n in
      Gc.add_finalizer hb final

type t = { foo: bool }

let () =
  let alloced_float = Unix.gettimeofday () in
  let alloced_bool = alloced_float > 0.0 in
  let alloced_string = String.create 4 in
  attach_finalizer "immediate int" 1;
  attach_finalizer "immediate float" 1.0;
  attach_finalizer "immediate variant" (`Foo "hello");
  attach_finalizer "immediate string" "hello world";
  attach_finalizer "immediate record" { foo=false };
  attach_finalizer "allocated float" alloced_float;
  attach_finalizer "allocated bool" alloced_bool;
  attach_finalizer "allocated variant" (`Foo alloced_bool);
  attach_finalizer "allocated string" alloced_string;
  attach_finalizer "allocated record" { foo=alloced_bool };
  Gc.compact ();
  never_returns (Scheduler.go ())
```

Building and running this should show the following output.

```console
$ ocamlfind ocamlopt -package core -package async -thread \
  -o finalizer -linkpkg finalizer.ml
$ ./finalizer
       immediate int: FAIL
     immediate float: FAIL
   immediate variant: FAIL
    immediate string: FAIL
    immediate record: FAIL
      allocated bool: FAIL
    allocated record: OK
    allocated string: OK
   allocated variant: OK
     allocated float: OK
```

The GC calls the finalization functions in the order of the deallocation. If
several values become unreachable during the same GC cycle, the finalisation
functions will be called in the reverse order of the corresponding calls to
`add_finalizer`.  Each call to `add_finalizer` adds to the set of functions
that are run when the value becomes unreachable. You can have many finalizers
all pointing to the same heap block if you wish.

After a garbage collection determines that a heap block `b` is unreachable, it
removes from the set of finalizers all the functions associated with `b`, and
serially applies each of those functions to `b`. Thus, every finalizer function
attached to `b` will run at most once.  However, program termination will
not cause all the finalizers to be run before the runtime exits.

The finalizer can use all features of OCaml, including assignments that make
the value reachable again and thus prevent it from being garbage collected. It
can also loop forever, which will cause other finalizers to be interleaved with
it.  

<note>
<title>Production note</title>

This chapter contains significant contributions from Stephen Weeks.

</note>

