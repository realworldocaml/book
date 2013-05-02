# Concurrent Programming with Async

The logic of building programs that interact with the outside world is
often dominated by waiting: waiting for the click of a mouse, or for
data to be fetched from disk, or for space to be available on an
outgoing network buffer.  Even mildly sophisticated sophisticated
interactive applications are typically _concurrent_, needing to wait
for multiple different events at the same time, responding immediately
to whatever event happens first.

A common approach to concurrency is to use preemptive system threads,
which is the most common solution in languages like Java or C#.  In
this model, each task that may require simultaneous waiting is given
an operating system thread of its own, so it can block without
stopping the entire program.  Other language runtimes such as
Javascript are single-threaded, and applications register function
callbacks to be triggered upon external events such as a timeout or
browser click.

Each of these mechanisms has its own trade-offs. Preemptive threads
require significant memory and other resources per thread.  Also, the
operating system can arbitrarily interleave the execution of
preemptive threads, requiring the programmer to carefully protect
shared resources with locks and condition variables, which can be
exceedingly error-prone.

Single-threaded event-driven systems, on the other hand, execute a
single task at a time and do not require the same kind of complex
synchronization that preemptive threads do.  However, the inverted
control structure of an event-driven program often means that your own
control flow has to be threaded awkwardly through the system's event
loop, leading to a maze of event callbacks.

This chapter covers the Async library, which offers a hybrid model
that aims to provide the best of both worlds, avoiding the performance
compromises and synchronization woes of preemptive threads without the
confusing inversion of control that usually comes with event-driven
systems.

## Async Basics

Consider a typical function for doing I/O in Core.

```ocaml
# In_channel.read_all;;
- : string -> string = <fun>
```

Since the function returns a concrete string, it has to block until
the read completes.  The blocking nature of the call means that no
progress can be made on anything else until the read is completed, as
you can see below.

```ocaml
# Out_channel.write_all "test.txt" ~data:"This is only a test.";;
- : unit = ()
# In_channel.read_all "test.txt";;
- : string = "This is only a test."
```

In Async, well-behaved functions never block.  Instead, they return a
value of type `Deferred.t` that acts as a placeholder that will
eventually be filled in with the result.  As an example, consider the
signature of the Async equivalent of `In_channel.read_all`.

```ocaml
# open Async.Std;;
# Reader.file_contents;;
- : string -> string Deferred.t = <fun>
```

Note that we opened `Async.Std`, which adds a number of new
identifiers and modules into our namespace that make using Async more
convenient.  Opening `Async.Std` is standard practice for writing
programs using Async, much like opening `Core.Std` is for using Core.

A deferred is essentially a handle to a value that may be computed in
the future.  As such, if we call `Reader.file_contents`, the resulting
deferred will initially be empty, as you can see by calling
`Deferred.peek` on the resulting deferred.

```ocaml
# let contents = Reader.file_contents "test.txt";;
val contents : string Deferred.t = <abstr>
# Deferred.peek contents;;
- : string option = None
```

The value in `contents` isn't yet determined in part because there's
nothing running that could do the necessary I/O.  When using Async,
processing of I/O and other events is handled by the Async scheduler.
When writing a stand-along program, you need to start the scheduler
explicitly, but utop knows about Async, and can start the scheduler
automatically.  More than that, utop knows about deferred values, and
when you type in an expression of type `Deferred.t`, it will make sure
the scheduler is running and block until the deferred is determined.
Thus, we can write:

```ocaml
# contents;;
- : string = "This is only a test.\n"
# Deferred.peek contents;;
- : string option = Some "This is only a test.\n"
```

In order to do real work with deferreds, we need a way of sequencing
deferred computations, which we do using `Deferred.bind`.  First,
let's consider the type-signature of bind.

```ocaml
# Deferred.bind ;;
- : 'a Deferred.t -> ('a -> 'b Deferred.t) -> 'b Deferred.t = <fun>
```

Thus, `Deferred.bind d f` takes a deferred value `d` and a function f
that is to be run with value of `d` once it's determined.  The call to
`Deferred.bind` returns a new deferred that becomes determined when
the deferred returned by `f` is determined.

Here's a simple use of bind for a function that replaces a file with
an uppercase version of its contents.

```ocaml
# let uppercase_file filename =
    let text = Reader.file_contents filename in
    Deferred.bind text (fun text ->
      Writer.save filename ~contents:(String.uppercase text))
  ;;
val uppercase_file : string -> unit Deferred.t = <fun>
# uppercase_file "test.txt";;
- : unit = ()
# Reader.file_contents "test.txt";;
- : string = "THIS IS ONLY A TEST."
```

Writing out `Deferred.bind` explicitly can be rather verbose, and so
`Async.Std` includes an infix operator for it: `>>=`.  Using this
operator, we can rewrite `uppercase_file` as follows.

```ocaml
# let uppercase_file filename =
    Reader.file_contents filename >>= fun text ->
    Writer.save filename ~contents:(String.uppercase text)
  ;;
val uppercase_file : string -> unit Deferred.t = <fun>
```

In the above we've dropped the parenthesis around the function on the
right-hand side of the bind, and we've didn't add a level of
indentation for the contents of that function.  This is standard
practice for using the bind operator.

Now let's look at another potential use of bind.  In this case, we'll
write a function that counts the number of lines in a file.

```ocaml
# let count_lines filename =
    Reader.file_contents filename >>= fun text ->
    List.length (String.split text ~on:'\n');;
  ;;
```

This looks reasonable enough, but when we try to compile it, we get
the following error.

```
Error: This expression has type int but an expression was expected of type
         'a Deferred.t
```

The issue here is that bind expects a function that returns a
deferred, but we've provided it a function that simply returns the
result.  To make these signatures match, we need a function for taking
an ordinary value and wrapping it in a deferred.  This function is a
standard part of Async, and is called `return`:

```
# return;;
- : 'a -> 'a Deferred.t = <fun>
# let three = return 3;;
val three : int Deferred.t = <abstr>
# three;;
- : int = 3
```

Using `return`, we can make `count_lines` compile.

```ocaml
# let count_lines filename =
    Reader.file_contents filename >>= fun text ->
    return (List.length (String.split text ~on:'\n'));;
  ;;
val count_lines : string -> int Deferred.t = <fun>
```

Together, `bind` and `return` form a design pattern in functional
programming known as a _monad_.  You'll run across this signature in
many applications beyond just threads.  Indeed, we already ran across
monads in [xref](#bind-and-other-error-handling-idioms).

Calling `bind` and `return` together is a fairly common pattern, and
as such there is a standard shortcut for it called `Deferred.map`,
which has the following signature:

```ocaml
# Deferred.map;;
- : 'a Deferred.t -> f:('a -> 'b) -> 'b Deferred.t = <fun>
```

and comes with its own infix equivalent, `>>|`.  Using it, we can
rewrite `count_lines` again a bit more succinctly:

```ocaml
# let count_lines filename =
    Reader.file_contents filename >>| fun text ->
    List.length (String.split text ~on:'\n');;
  ;;
val count_lines : string -> int Deferred.t = <fun>
```

## A simple TCP Echo Server

Now that we have the basics of Async under our belt, let's look at a
small complete stand-alone Async program. In particular, we'll write
an echo server, _i.e._, a program that accepts connections from
clients and spits back every line of text sent to it.

The first step is to create a function that can copy data from an
input to an output.  Here, we'll use Async's `Reader` and `Writer`
modules which provide a convenient abstraction for working with input
and output channels.

```ocaml
(* filename: echo.ml *)
open Core.Std
open Async.Std

(* Copy data from the reader to the writer, using the provided buffer
   as scratch space *)
let rec copy_blocks buffer r w =
  Reader.read r buffer
  >>= function
  | `Eof -> return ()
  | `Ok bytes_read ->
    Writer.write w buffer ~len:bytes_read;
    Writer.flushed w
    >>= fun () ->
    copy_blocks buffer r w
```

Bind is used in the above code to sequence the operations: first, we
call `Reader.read` to get a block of input, then, when that's complete
and if a new block was returned, we write that block to the writer.
Finally, we wait until the writer's buffers are flushed, waiting on
the deferred returned by `Writer.flushed`, at which point we recur.
If we hit an end-of-file condition, the loop is ended.  The deferred
returned by a call to `copy_blocks` becomes determined only once the
end-of-file condition is hit.

One important aspect of how this is written is that it uses
_pushback_, which is to say that if the writer can't make progress
writing, the reader will stop reading.  If you don't implement
pushback in your servers, then a stopped client can cause your program
to leak memory, since you'll need to allocate space for the data
that's been read in but not yet written out.

`copy_blocks` provides the logic for handling a client connection, but
we still need to set up a server to receive such connections and
dispatch to `copy_blocks`.  For this, we'll use Async's `Tcp` module,
which has a collection of utilities for creating simple TCP clients
and servers.

```ocaml
(** Starts a TCP server, which listens on the specified port, invoking
    copy_lines every time a client connects. *)
let run () =
  let buffer = String.create (16 * 1024) in
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port 8765)
      (fun _addr r w -> copy_blocks buffer r w)
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t)
```

The result of calling `Tcp.Server.create` is a `Tcp.Server.t`, which
is a handle to the server that lets you shut the server down.  We
don't use that functionality here, so we explicitly ignore [server] to
suppress the unused-variables error.  We put in a type annotation
around the ignored value to make the nature of the value we're
ignoring explicit.

The most important argument to `Tcp.Server.create` is the final one,
which is the client connection handler.  Notably, the above code does
nothing explicit to close down the client connections when the
communication is done.  That's because the server will automatically
shut down the connection once the deferred returned by the handler
becomes determined.

Finally, we need to initiate the server and start the Async scheduler.

```ocaml
(* Call [run], and then start the scheduler *)
let () =
  run ();
  never_returns (Scheduler.go ())
```

One of the most common newbie errors with Async is to forget to run
the scheduler.  It can be a bewildering mistake, because without the
scheduler, your program won't do anything at all; even calls to
`printf` won't actually reach the terminal.

It's worth noting that even though we didn't spend much explicit
effort on thinking about multiple clients, this server is able to
handle many concurrent clients without further modification.

Now that we have the echo server, we can try it out using `netcat`.

```
echo_server $ ./echo.native &
[1] 25030
echo_server $ nc 127.0.0.1 8765
This is an echo server
This is an echo server
It repeats whatever I write.
It repeats whatever I write.
```

<note><title>Functions that never return</title>

You might wonder what's going on with the call to `never_returns`
above.  `never_returns` is an idiom that comes from `Core` that is
used to mark functions that don't return.  Typically, a function that
doesn't return is inferred as having return type `'a`.

```ocaml
# let rec loop_forever () = loop_forever ();;
val loop_forever : unit -> 'a = <fun>
# let always_fail () = assert false;;
val always_fail : unit -> 'a = <fun>
```

This can be surprising when you call a function like this expecting it
to return unit, and really it never returns.  The type-checker won't
necessarily complain in such a case.

```ocaml
# let do_stuff n =
    let x = 3 in
    if n > 0 then loop_forever ();
    x + n
  ;;
val do_stuff : int -> unit = <fun>
```

With a name like `loop_forever`, the meaning is clear enough in this
case.  But with something like `Scheduler.go`, the fact that it never
returns is less clear, and so we use the type-system to make it more
explicit by giving it a return type of `never_returns`.  To make it
clearer how this works, let's do the same trick with `loop_forever`.

```ocaml
# let rec loop_forever () : never_returns = loop_forever ();;
val loop_forever : unit -> never_returns = <fun>
```

The type `never_returns` is uninhabited, so a function can't return a
value of type `never_returns`, which means only functions that never
return can have it as their return type!  Now, if we rewrite our
`do_stuff` function, we'll get a helpful type error.

```ocaml
# let do_stuff n =
    let x = 3 in
    if n > 0 then loop_forever ();
    x + n
  ;;
Error: This expression has type unit but an expression was expected of type
         never_returns
```

We can resolve the error by calling the function `never_returns`.

```ocaml
# never_returns;;
- : never_returns -> 'a = <fun>
# let do_stuff n =
    let x = 3 in
    if n > 0 then never_returns (loop_forever ());
    x + n
  ;;
val do_stuff : int -> int = <fun>
```

Thus, we got the compilation to go through by explicitly marking in
the source that the call to `loop_forever` never returns.

</note>

## Improving the echo server

Let's try to go a little bit farther with our echo server.  Let's walk
through a few small improvements:

- Add a proper command-line interface with `Command`
- Add a flag to specify the port to listen on, and a flag to make the
  server echo back the capitalized version of whatever was sent to it.
- Simplify the code using Async's `Pipe` interface.

Here's the improved code below.

```ocaml
let run ~uppercase ~port =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port port)
      (fun _addr r w ->
        Pipe.transfer (Reader.pipe r) (Writer.pipe w)
           ~f:(if uppercase then String.uppercase else Fn.id))
  in
  ignore (host_and_port : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t);
  Deferred.never ()

let () =
  Command.async_basic
    ~summary:"Start an echo server"
    Command.Spec.(
      empty
      +> flag "-uppercase" no_arg
        ~doc:" Convert to uppercase before echoing back"
      +> flag "-port" (optional_with_default 8765 int)
        ~doc:" Port to listen on (default 8765)"
    )
    (fun uppercase port () -> run ~uppercase ~port)
  |> Command.run
```

The most notable change in this function is the use of Async's `Pipe`
API.  A `Pipe` is a communication channel that's used for connecting
different parts of your program.  You can think of it as a
consumer/producer queue that uses deferreds for communicating when the
pipe is ready to be read from or written to.

Pipes are created in connected read/write pairs, as you can see below.

```ocaml
# let (r,w) = Pipe.create ();;
val r : '_a Pipe.Reader.t = <abstr>
val w : '_a Pipe.Writer.t = <abstr>
```

`r` and `w` are really just read and write handles to the same
underlying object.  Note that `r` and `w` have weakly polymorphic
types.  That's because a pipe is mutable and so can contain elements
of only one type, which will be settled by the compiler once we try to
use the pipe for anything.

If we just try and write to the writer, we'll see that we block
indefinitely in utop.  You can break out of the wait by hitting
`Control-C`.

```ocaml
# Pipe.write w "Hello World!";;
Interrupted.
```

The deferred returned by write completes on its own once the value
written into the pipe has been read out:

```ocaml
# let (r,w) = Pipe.create ();;
val r : '_a Pipe.Reader.t = <abstr>
val w : '_a Pipe.Writer.t = <abstr>
# let write_complete = Pipe.write w "Hello World!";;
val write_complete : unit Deferred.t = <abstr>
# Pipe.read r;;
- : [ `Eof | `Ok of string ] = `Ok "Hello World!"
# write_complete;;
- : unit = ()
```

In the function `run` above, we're taking advantage of one of the many
utility functions provided for pipes in the `Pipe` module.  In
particular, we're using `Pipe.transfer` to set up a process that takes
data from a reader-pipe and moves it to a writer-pipe.  Here's the
type of `Pipe.transfer`:

```ocaml
# Pipe.transfer;;
- : 'a Pipe.Reader.t -> 'b Pipe.Writer.t -> f:('a -> 'b) -> unit Deferred.t =
<fun>
```

The two pipes being connected are generated by the `Reader.pipe` and
`Writer.pipe` call respectively.  Note that pushback is preserved
throughout the process, so that if the writer gets blocked, the the
writer's pipe will stop pulling data from the reader's pipe, which
will prevent the reader from reading in more data.

The command-line parsing for this program is based on the `Command`
library that we introduced in [xref](#command-line-parsing).  When you
open `Async.Std`, the `Command` module has added to it the `async_basic`
call:

```ocaml
# Command.async_basic;;
- : summary:string ->
    ?readme:(unit -> string) ->
    ('a, unit -> unit Deferred.t) Command.Spec.t -> 'a -> Command.t
= <fun>
```

This differs from the ordinary `Command.basic` call in that the main
function must return a `Deferred.t`, and that the running of the
command (using `Command.run`) automatically starts the async
scheduler, without requiring an explicit call to `Scheduler.go`.

