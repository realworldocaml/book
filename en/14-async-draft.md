# Concurrent Programming with Async

When you start building OCaml code that interfaces with external systems,
you'll soon need to handle concurrent operations. A web server sending a large
file to many clients, or a GUI waiting for a mouse click are examples of
applications that need to keep track of multiple threads.  These threads can
block waiting for input, and the runtime system has to wake them up in response
to new data, and do so efficiently across thousands of connections.

In some programming languages such as Java or C#, you've probably used
preemptive system threads.  Other languages such as Javascript are
single-threaded, and an application must register function callbacks to be
triggered upon an external event (such as a timeout or browser click).  Both
mechanisms have tradeoff: preemptive threads can be memory hungry and require
careful locking, but events quickly
descend into a maze of callbacks that are hard to read and debug.

The Async library offers an interesting hybrid that lets you write
straight-line blocking OCaml code that scales very well. Async internally
converts this code into a single event loop.  ``Threads'' in Async are normal
OCaml heap-allocated values, without any runtime magic, and their number is
limited only by your available main memory. 

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
the ultimate type of the thread once it has completed.  This return value
cannot be used directly while it is wrapped in a `Deferred.t` as it may not be
available yet.  Instead, we can *bind* a function closure to be called once the
value is eventually ready.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let x = return 5 ;;
val x : int Deferred.t = <abstr>
# let y = Deferred.bind x (fun a -> return (string_of_int a)) ;;
val y : string Deferred.t = <abstr>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here, we've bound a function to `x` that will convert the `int` to a `string`.
Notice that while both `x` and `y` share a common `Deferred.t` type, their type
variables differ and so they cannot be interchangably used except in
polymorphic functions.  This is very useful when refactoring large codebases as
you can tell if any function will block simply by the presence of an
`Deferred.t` in the signature.

Let's examine the function signatures of `bind` and `return` more closely.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# return ;;
- : 'a -> 'a Deferred.t = <fun>
# Deferred.bind ;;
- : 'a Deferred.t -> ('a -> 'b Deferred.t) -> 'b Deferred.t = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`return`, `bind` and the `Deferred.t` type all contain polymorphic type
variables that are inferred based on how they are used in your code.  In
`bind`, the `'a` type of the argument passed to the callback *must* be the same
as the `'a Deferred.t` of the input thread, preventing runtime mismatches
between callbacks.  Both `bind` and `return` form a design pattern in
functional programming known as *monads*, and you will run across this
signature in many applications beyond just threads.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let x = return 5 ;;
val x : int Deferred.t = <abstr>
# x >>= (fun a -> return (string_of_int a)) ;;
val - : string Deferred.t = <abstr>
# x >>| string_of_int ;;
val - : string Deferred.t = <abstr>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

TODO explain `>>=` and `>>|`

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Lwt_unix.run ;;
- : 'a Lwt.t -> 'a = <fun>
# Lwt_unix.run y;;
- : string = "5"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`Lwt_unix.run` executes a thread until it completes, and returns a normal OCaml
value.  This usually forms the main loop of your program, as threads cannot be
unblocked unless they run within the `run` function. 

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


