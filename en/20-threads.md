## Concurrency

Concurrency is another tool OCaml programmers can use to simplify
programs in certain cases.  In OCaml, threads do not introduce
parallelism; only one thread may be running at a time.  However,
threads can be used to simplify control flow -- a program can devote
some threads to reading input, others for performing work, others for
producing output, etc.

The OCaml standard library supports threads, where individual threads
of control can be created that run concurrently (but only one thread
at a time), and context switches are _involuntary_, meaning that a
thread may be preempted at any time.

When threads share imperative state, this gives rise to the standard
synchronization issues, where multiple threads may be mutating shared
state at the same time.  To illustrate, let's write a program with two
threads that increment a shared reference cell concurrently.

```ocaml
let value = ref 0

let loop () =
   for i = 1 to 3 do
     let i = !value in
     Printf.printf "i = %d\n" i;
     flush stdout;
     value := i + 1
   done

let thread1 = Thread.create loop ();;
let thread2 = Thread.create loop ();;

Thread.join thread1;;
Thread.join thread2;;

Printf.printf "value = %d\n" !value
```

The reference cell `value` holds a shared value that is incremented 10
times, in a loop, by the `loop` function.  Each iteration of the loop
prints the current value, then assigns the new value.  We create two
threads with the `Thread.create` expressions, then use `Thread.join`
to block until the threads terminate.

The exact behavior of the program is nondeterminstic -- it depends on
the relative sopeed of the two theads.  One output is listed below.

```ocaml
i = 0
ii  ==  01

ii  ==  12

i = 2
value = 3
```

This represents a nearly perfect interleaving of the thread
executions.  Each thread fetches the value from the `value` reference
cell, prints the value, then performs the assignment.  Since both
threads effectively run in lockstep, the final value in the `value`
reference cell is the same as if there were just one thread running.

If this is not the behavior that was expected, one solution is to use
a `Mutex` to ensure that the increment operation is atomic.  We can
allocate a lock with `Mutex.create`, then acquire the lock in the loop
body.

```ocaml
let value = ref 0
let mutex = Mutex.create ()

let loop () =
   for i = 1 to 3 do
     Mutex.lock mutex;
     let i = !value in
     Printf.printf "i = %d\n" i;
     flush stdout;
     value := i + 1;
     Mutex.unlock mutex
   done
```

When we run this program, it produces a deterministic output.

```
i = 0
i = 1
i = 2
i = 3
i = 4
i = 5
value = 6
```

### Dealing with concurrency

In general the interaction of concurrency with imperative programs causes
problems with races.  There are many techniques you can use to address the
issue.

* Do not use assignment, mutable data structures, or perform
  input/output in threads.

* Use _cooperative_ multitasking, where only one thread runs at a
  time, and context switches are _voluntary_.  This is the dominant
  model in `Async`.

* Do not share mutable data between threads.  In practice, this
  usually includes explicit communication channels between threads
  that otherwise have isolated state.

* Give in, and use threads, and the standard synchronization toolkit
  that comes with OCaml, including locks (`Mutex`), condition
  variables, etc.

Out of all of these choices, the simplest one is to use the `Async`
model and cooperative multitasking.  However, let's go ahead and work
through some examples of using traditional concurrent programming
using locks and other synchronization primitives to build a
concurrency library.

### Concurrent hash tables

Let's extend our hash table example to support concurrency.  To begin,
let's first give the signature of the module we will implement.  The
table has operations to add, remove, and find elements, and it also
supports imperative iterators.

```ocaml
module Concurrent_dictionary : sig
  type ('a, 'b) t

  val create : unit -> ('a, 'b) t
  val add : ('a, 'b) t -> key:'a -> data:'b -> unit
  val find : ('a, 'b) t -> key:'a -> 'b option
  val remove : ('a, 'b) t -> key:'a -> unit
  val iterator : ('a, 'b) t -> ('a * 'b) iterator
end
```

We'll use the same basic construction that we used to implement the `Dictionary`,
where a hash table contains an array of buckets.  In addition we'll add locking
to ensure that concurrent operations do not interfere.  To reduce lock
contention, we'll use an array of locks to partition the table into multiple
parts.  If operations are uniformly distributed, this should reduce lock
contention.

```ocaml
  type ('a, 'b) element = {
    key : 'a;
    mutable value : 'b
  }
  type ('a, 'b) t = {
    locks : Mutex.t array;
    mutable buckets : ('a, 'b) element list array
  }

  let num_locks = 32
  let num_buckets = 256

  let create () = {
    locks = Array.init num_locks (fun _ -> Mutex.create ());
    buckets = Array.create num_buckets []
  }
```

Each `element` is a key/value pair, where the value is mutable so that
the `add` function can mutate it in place.  For this implementation,
we'll use 32 locks, and start with 256 buckets.

Each bucket is an _association list_, meaning that it is list of key/value pairs
that implement a dictionary.  We can start the implementation by defining
dictionary operations for association lists.  The function `find_assoc` finds
the value associated with a key, and `remove_assoc_exn` removes an association.
The remove function raises an exception `Not_found` if the list does not contain
the association.  We'll use this to optimize removal.

```ocaml
  let rec find_assoc key = function
  | element :: tl ->
       if element.key = key then
          Some element
       else
          find_assoc key tl
  | [] -> None

  let rec remove_assoc_exn key = function
  | element :: tl ->
       if element.key = key then
          tl
       else
          element :: remove_assoc_exn key tl
  | [] -> raise Not_found
```

The locks are intended to partition the table into multiple sub-parts, where
each lock provides synchronization for a contiguous range of buckets.  For
synchronization we define a function `synchronize` that takes a bucket index and
a function, and evaluates the function with the bucket lock acquired, releasing
the lock before returning.

```ocaml
  let synchronize table index f =
    let lock = table.locks.(index * num_locks / num_buckets) in
    Mutex.lock lock;
    let result = f () in
    Mutex.unlock lock;
    result
```

Note that the `synchronize` function is _not_ exception-safe, meaning
that if evaluation of `f ()` raises an exception, the lock will not be
released.  An exception-safe version would catch all exceptions; when
an exception is raised, the lock would be released, and the exception
re-raised.

```ocaml
  let synchronize_exn table index f =
    let lock = table.locks.(index * num_locks / num_buckets) in
    Mutex.lock lock;
    try let result = f () in Mutex.unlock lock; result with
      exn -> Mutex.unlock lock; raise exn
```

To add a new entry to the table, the `add` function acquires the
bucket lock, then uses `find_assoc` to look for an existing
association.  If one is found, the `value` is updated in-place to the
new value.  Otherwise, a new entry is added to the beginning of the
bucket.

```ocaml
  let add table ~key ~data =
    let hash = Hashtbl.hash key in
    let index = hash mod num_buckets in
    let buckets = table.buckets in
    synchronize table index (fun () ->
          match find_assoc key buckets.(index) with
          | Some element ->
               element.value <- data
          | None ->
               buckets.(index) <- { key = key; value = data } :: buckets.(index))
```

Removing an element from the table is similar.  If here is a previous entry in
the table, the entry is removed.  Otherwise, the `remove_assoc_exn` function
raises `Not_found`, and we leave the bucket unchanged.  The exception is an
optimization to avoid copying the entire list in this case.

```ocaml
  let remove table ~key =
    let hash = Hashtbl.hash key in
    let index = hash mod num_buckets in
    let buckets = table.buckets in
    synchronize table index (fun () ->
      try buckets.(index) <- remove_assoc_exn key buckets.(index) with
      | Not_found -> ())
```

The function to find an association in the table is similar -- we jsut
find the entry in the table and return the value part.  However, this
particular implementation is somewhat more subtle, because it omits
the synchronization step, examining the bucket _without_ acquiring the
lock.

```ocaml
  let find table ~key =
    let hash = Hashtbl.hash key in
    let index = hash mod num_buckets in
    (* Unsynchronized *)
    match find_assoc key table.buckets.(index) with
    | Some element -> Some element.value
    | None -> None
```

From a performance perspective, this is clearly a win, because
retrieving elements from the table has no locking at all.  But why is
it valid?

The reasoning has to do with two things: 1) the semantics we expect
from the table, and 2) the OCaml memory model.  Ideally, the semantics
we would expect is _sequential semantics_, meaning that all memory
operations are processed in _some_ sequence that is compatible with
the order in which they were performed in each thread.  Thus, if some
thread adds two entries for keys `K1` and `K2` in sequential order,
then all other threads will see either, 1) neither entry, or 2) a
entry for `K1`, or 3) a entry for both `K1` and `K2`, but it will
_not_ see an entry for `K2` without also having an entry for `K1`.

Unfortunately, for some processor architectures, primary memory does
not have sequential semantics, due to caching and other effects.
Fortunately for us, OCaml does provide sequential memory semantics due
to its threading model where: 1) only one thread executes at a time,
and 2) there is a memory barrier the prevents reordering of thread
context switches and memort operations, and 3) the compiler does not
reorder memory operations in ways that would violate sequential memory
semantics.

Note, OCaml does not a _guarantee_ this semantics.  The OCaml
implementation may change to support parallelism.  If it does, the
memory semantics will change accordingly.  The simplest fix is just to
synchronize the access.  Performance of `find` operations will
decrease somewhat due to contention.

```ocaml
  let synchronized_find table ~key =
    let hash = Hashtbl.hash key in
    let index = hash mod num_buckets in
    synchronize table index (fun () ->
      (find_assoc key table.buckets.(index)).value)
```

For the final part of the implementation, let's define imperative
iteration.  The iterator object contains a bucket index, and the field
`elements` refers to some suffix of the list stored in the bucket.
The `value` method returns the current elements, and the `next` method
advances the `elements` field.  The method `normalize` is used to
maintain the invariant that the `elements` field always refers to a
value in the table unless the iterator has advanced past the final
element.  The `remove` method removes the current element from the
bucket in which it is stored.

```ocaml
  let rec remove_element_exn elements = function
  | (hd :: tl) as elements' ->
       if elements' == elements then
          tl
       else
          hd :: remove_element_exn elements tl
  | [] -> raise Not_found

  let iterator table =
    let buckets = table.buckets in
    object (self)
      val mutable index = 0
      val mutable elements = buckets.(0)
      method has_value = elements <> []
      method value =
        match elements with
        | { key = key; value = value } :: _ -> key, value
        | [] -> raise (Invalid_argument "value")
      method next =
        elements <- List.tl elements;
        self#normalize
      method remove =
        synchronize table index (fun () ->
          try buckets.(index) <- remove_element_exn elements buckets.(index) with
            Not_found -> ());
        self#next
      method private normalize =
        while elements = [] && index < num_buckets do
          index <- index + 1;
	  elements <- buckets.(index)
        done
      initializer self#normalize
    end
```

The iterator methods are all unsychronized except the method `remove`, which
mutates the bucket.  As a consequence, it means that hash operations that add
and remove elements from the list can happen concurrently with iteration.
Again, this is great from a performance perspective, but it means that iteration
has non-sequential semantics.  In particular, whenever iteration enters a new
bucket, subsequent concurrent operations that add new elements or remove old
ones from that bucket have _no effect_ on the iteration.  Iteration advances
through that bucket as if it were unchanged.

One advantage of this relaxed iteration semantics is peformance, since
iteration is largely unsynchronized.  Another advantage is that
deadlock is less likely.  If we were to _lock_ the bucket during
iteration, then changes to that bucket would not be allowed during
iteration (even by the iterating thread).  We might allow lock
recursion to allow mutations by the iterating thread, but in general
the synchronization might involve multiple threads, resulting in
deadlock.  Lock-free iteration ensures that the
`Concurrent_dictionary` will not be involved in a deadlock cycle.

