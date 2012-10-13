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
straight-line blocking OCaml code that scales well without using preemptive
threading. Async "threads" are co-operative and never preempt each other, and
the library internally converts blocking code into a single event loop.  The
threads are normal OCaml heap-allocated values (without any runtime magic!) and
are therefore very fast to allocate. Concurrency is mostly limited only by your
available main memory, or operating system limits on non-memory resources such
as file descriptors.

Lets begin by constructing a simple thread. Async follows the Core convention
and provides an `Async.Std` that provides threaded variants of many standard
library functions.

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
into the `y` variable, and the subsequent closure builds a new future which
contains the string value.  It can be a little verbose to keep calling `bind`
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

## Timing and Thread Composition

Our examples so far have been with static threads, and now we'll look at how to
coordinate multiple threads and timeouts.  Lets write a program that spins off
two threads, each of which sleep for some random amount of time, and then one
prints "Heads" and the other "Tails", and finally prints "Finished" before
exiting.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
open Lwt
open Printf

let main () =
  bind (join [
    bind (Lwt_unix.sleep (Random.float 3.0)) (fun () ->
      print_endline "Heads";
      return ()
    );
    bind (Lwt_unix.sleep (Random.float 3.0)) (fun () ->
      print_endline "Tails";
      return ()
    );
  ]) (fun () ->
    print_endline "Finished";
    return ()
  )

let _ = Lwt_unix.run (main ())
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a full code example that you can compile via (???).  The `bind`
function is joined by couple of new functions.  `Lwt_unix.sleep` puts a thread
to sleep for a given time, and `join` takes a list of threads and waits for all
of them to terminate. If at least one thread fails then `join` fails with the
same exception as the first to fail *after* all threads terminate.  When run,
this program immediately spawns two coin threads, and waits on them to complete
before calling the final "Finished" closure.

The control flow above is somewhat hard to follow due to all the nested binds,
and so Lwt provides infix operators with the same behaviour.

Function    Operator  Behaviour
--------    --------  ---------
bind        >>=       Wait for thread to finish, and apply return to new thread
join        <&>       Wait for two threads to finish and return unit
choose      <?>       Wait for the first thread to finish, cancel rest
map         >|=       Map a non-blocking function over a blocking thread

We can now rewrite the earlier coin-flipping example using these operators.
Notice that we can explicitly name threads simply by binding them via `let`,
and they execute in parallel until joined together.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
let main () =
  let t1 =
    Lwt_unix.sleep (Random.float 3.0) >>= fun () ->
    return (print_endline "Heads")
  in
  let t2 = 
    Lwt_unix.sleep (Random.float 3.0) >>= fun () ->
    return (print_endline "Tails")
  in
  (t1 <&> t2) >>= fun () ->
  return (print_endline "Finished")
~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Syntax Extensions

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
let main () =
  let t1 =
    Lwt_unix.sleep (Random.float 3.0) >>
    return (print_endline "Heads")
  in
  let t2 = 
    Lwt_unix.sleep (Random.float 3.0) >>
    return (print_endline "Tails")
  in
  lwt () = t1 <&> t2 in
  return (print_endline "Finished")
~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Cancellation

`choose` behaves as the first thread in l to terminate. If several threads are already terminated, one is chosen at random.
Mixing normal exceptions and Lwt exceptions is bad.

## A simple TCP Echo Server

Not using Lwt_daemon, but directly. This will be UNIX-only from this stage on, hmm...

## Onto an HTTP Server

Describe cohttp (much simpler than Ocsigen at this stage).

## Binding to the Github API

Show how we can use a monadic style to bind to the Github API and make simple JSON requests/responses.

<sidebar><title>A Note on Portability</title>

Explain libev and why its needed here.

</sidebar>


