# Concurrent Programming with Async

When you start building OCaml code that interfaces with external systems,
you'll soon need to handle concurrent operations. Consider the case of a web
server sending a large file to many clients, or a GUI waiting for a mouse
clicks.  These applications must block threads of control flow waiting for
input, and the runtime has to resume these threads when new data arrives.
Efficiency is an important consideration on busy systems withs thousands of
connections, but equally important is readable source code where the control
flow of the program is obvious at a glance.

In some programming languages such as Java or C#, you've probably used
preemptive system threads, where multiple connections are tracked using
operating system threads.  Other languages such as Javascript are
single-threaded, and applications must register function callbacks to be
triggered upon external events (such as a timeout or browser click).  Both
mechanisms have tradeoffs. Preemptive threads can be memory hungry and require
careful locking due to unpredictable interleaving. Event-driven systems can
descend into a maze of callbacks that are hard to read and understand.

The Async OCaml library offers an interesting hybrid model that lets you write
straight-line blocking code that scales well without using preemptive
threading. Async "threads" are co-operative and never preempt each other, and
the library internally converts blocking code into a single event loop.  The
threads are normal OCaml heap-allocated values (without any runtime magic!) and
are therefore very fast to allocate. Concurrency is mostly limited only by your
available main memory, or operating system limits on non-memory resources such
as file descriptors.

Lets begin by constructing a simple thread. Async follows the Core convention
and provides an `Async.Std` that provides threaded variants of many standard
library functions.  The examples throughout this chapter assume that `Async.Std`
is open in your environment.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# require "async.unix" ;;
# open Async.Std ;;
# return 5 ;;
- : int Deferred.t = <abstr>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The basic type of an Async thread is a `Deferred.t`, which can be constructed
by the `return` function.  The type parameter (in this case `int`) represents
the ultimate type of the thread once it has completed in the future.  This
return value cannot be used directly while it is wrapped in a `Deferred.t` as
it may not be available yet.  Instead, we `bind` a function closure that is
called once the value is eventually ready.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let x = return 5 ;;
val x : int Deferred.t = <abstr>
# let y = Deferred.bind x (fun a -> return (string_of_int a)) ;;
val y : string Deferred.t = <abstr>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here, we've bound a function to `x` that will convert the `int` to a `string`.
Notice that while both `x` and `y` share a common `Deferred.t` type, their type
variables differ and so they cannot be interchangably used except in
polymorphic functions.  This is useful when refactoring large codebases, as you
can tell if any function will block simply by the presence of an `Deferred.t`
in the signature.

Let's examine the function signatures of `bind` and `return` more closely.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# return ;;
- : 'a -> 'a Deferred.t = <fun>
# Deferred.bind ;;
- : 'a Deferred.t -> ('a -> 'b Deferred.t) -> 'b Deferred.t = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`return`, `bind` and the `Deferred.t` type all contain polymorphic type
variables (the `'a`) which represent the type of the thread, and are inferred
based on how they are used in your code. The `'a` type of the argument passed
to the `bind` callback *must* be the same as the `'a Deferred.t` of the input
thread, preventing runtime mismatches between thread callbacks.  Both `bind` and
`return` form a design pattern in functional programming known as *monads*, and
you will run across this signature in many applications beyond just threads.

_(avsm: do we talk about Monads earlier in the Core chapter? I presume we do,
since the Option monad is very useful)

Binding callbacks is to deferred values is the most common way to compose
blocking operations, and inline operators are provided to make it easier to use.
In the fragment below, we see `>>=` and `>>|` used in similar ways to convert
an integer into a string:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let x = return 5 ;;
val x : int Deferred.t = <abstr>
# x >>= fun y -> return (string_of_int y) ;;
val - : string Deferred.t = <abstr>
# x >>| string_of_int ;;
val - : string Deferred.t = <abstr>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `>>=` operator is exactly the same as `bind` and unpacks the integer future
into the `y` variable. The subsequent closure receives the unpacked integer and
builds a new string future.  It can be a little verbose to keep calling `bind`
and `return`, and so the `>>|` operator maps a non-Async function across a
future value.  In the second example, the future value of `x` is mapped to
`string_of_int` directly, and the result is a `string` future.

Async threads can be evaluated from the toplevel by wrapping them in
`Thread_safe.block_on_async_exn`, which spawns a system thread that waits until
a result is available.  The `utop` top-level automatically detects `Deferred.t`
types that are entered interactively and wraps them in this function for you
automatically.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let fn () = return 5 >>| string_of_int ;;
val fn : unit -> string Deferred.t = <abstr>
# Thread_safe.block_on_async_exn fn ;;
- : string = "5"
# fn () ;;
- : string = "5"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the second evaluation of `fn`, the top-level detected the return type of
a future and evaluated the result into a concrete string.

(_avsm_: this utop feature not actually implemented yet for Async, but works for Lwt)

## Timing and Thread Composition

Our examples so far have been with static threads, and now we'll look at how to
coordinate multiple threads and timeouts.  Let's write a program that spawns
two threads, each of which sleep for some random time and return either
"Heads" or "Tails", and the quickest thread returns its value.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let flip () =
  let span = Time.Span.of_sec 3.0 in
  let span_heads = Time.Span.randomize span ~percent:0.75 in
  let span_tails = Time.Span.randomize span ~percent:0.75 in
  let coin_heads =
    Clock.after span_heads
    >>| fun () ->
    "Heads!", span_heads, span_tails
  in
  let coin_tails =
    Clock.after span_tails
    >>| fun () ->
    "Tails!", span_heads, span_tails
  in
  Deferred.any [coin_heads; coin_tails] ;;
val flip : unit -> (string * Time.Span.t * Time.Span.t) Deferred.t = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This introduces a couple of new time-related Async functions. The `Time` module
contains functions to express both absolute and relative temporal
relationships.  In our coin flipping example, we create a relative time span of
3 seconds, and then permute it randomly twice by 75%.  We then create two
threads, `coin_heads` and `coin_tails` which return after their respective
intervals.  Finally, `Deferred.any` waits for the first thread which completes
and returns its value, ignoring the remaining undetermined threads.

Both of the threads encode the time intervals in their return value so that you
can can easily verify the calculations (you could also simply print the time
spans to the console as they are calculated and simplify the return types).
You can see this by executing the `flip` function at the toplevel a few times.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Thread_safe.block_on_async_exn flip ;;
# - : string * Time.Span.t * Time.Span.t = ("Heads!", 2.86113s, 3.64635s) 
# Thread_safe.block_on_async_exn flip ;;
# - : string * Time.Span.t * Time.Span.t = ("Tails!", 4.44979s, 2.14977s)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `Deferred` module has a number of other ways to select between multiple
threads, such as:

Function    # Threads  Behaviour
--------    ---------  ---------
both        2          Combines both threads into a tuple and returns both values.
any         list       Returns the first thread that becomes determined.
all         list       Waits for all threads to complete and returns their values.
all_unit    list       Waits for all `unit` threads to complete and returns `unit`.
peek        1          Inspects a single thread to see if it is determined yet.

Try modifying the `Deferred.any` in the above example to use some of the other
thread joining functions above, such as `Deferred.both`.

### Cancellation

## A simple TCP Echo Server

## Onto an HTTP Server

## Binding to the Github API

Show how we can use a monadic style to bind to the Github API and make simple JSON requests/responses.

<sidebar><title>A Note on Portability</title>

Explain libev and why its needed here.

</sidebar>


