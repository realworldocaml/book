# Concurrent Programming with Lwt

As soon as you start building OCaml code that interfaces with external systems,
you'll need to handle concurrent, blocking operations. A web server sending a
large file to many clients or a GUI waiting for a mouse click are both
applications of this sort.  These applications need to keep track of how
blocking code is woken up in response to external I/O, and do so efficiently
for many instances of these network connections or GUI elements.

In some programming languages such as Java or C#, you've probably used preemptive
system threads to handle such operations.  Other languages like Javascript are
single-threaded, and an application must register callbacks to be triggered
upon an external event such as a timeout or a browser click.
Both methods have tradeoffs: threads are more memory hungry and require careful
locking, but event callbacks quickly descend into a spaghetti of callbacks.

In OCaml, the Lwt library offers an interesting hybrid that lets you write
straight-line blocking code that is internally evaluated via a single
event-loop.  Lwt threads are simply normal OCaml heap-allocated values, without
any runtime magic, that hide internal state using the abstract types described
earlier.  These lightweight threads are limited only by your main memory and
can be interfaced with a variety of network, storage and graphical outputs
(including web browsers).  In this chapter, we'll focus on the basics of Lwt
and how to use it for building network services. Later, we'll describe more
exotic platforms such compiling the same code to Javascript (Chapter {???}).

Lets begin by constructing our first thread. `Lwt` is just a normal library, so open
it in your toplevel and create your first thread:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# require "lwt" ;;
# open Lwt ;;
# return 5 ;;
- : int Lwt.t = <abstr>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`return` constructs a thread that returns immediately, and above we've built
one that returns a constant 5.  Notice that the return type is not a normal
`int`, but instead `int Lwt.t`. The additional type parameter marks the value
as a lightweight thread, which can internally be blocked, raising an exception,
or completed.  We can only *use* the return value by binding a further function
to be invoked after the input thread has finished.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let x = return 5 ;;
val x : int Lwt.t = <abstr>
# let y = bind x (fun a -> return (string_of_int a)) ;;
val y : string Lwt.t = <abstr>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here, we've bound a function to `x` that will convert it to a string.  Notice
that while both `x` and `y` share a common `Lwt.t` type, their type variables
differ based on return type of the thread.  This can be really useful in large
codebases, as you can tell if any function will block simply by the presence
of an `Lwt.t` in the signature.

Lets examine the function signatures of `bind` and `return` more closely.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# return ;;
- : 'a -> 'a Lwt.t = <fun>
# bind ;;
- : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`return`, `bind` and the `Lwt.t` type all contain polymorphic type variables
that are automatically inferred based on how they are used in your code.
`bind` is particularly interesting as the the callback argument `'a` *must* be
the same as the return type of the input thread, preventing runtime mismatches
between callbacks.  Both `bind` and `return` form a design pattern in
functional programming known as *monads*, and you will run across this
signature in many applications beyond just threads.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# #require "lwt.unix" ;;
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


