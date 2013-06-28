# Concurrent Programming with Async

The logic of building programs that interact with the outside world is
often dominated by waiting: waiting for the click of a mouse, or for
data to be fetched from disk, or for space to be available on an
outgoing network buffer.  Even mildly sophisticated interactive
applications are typically _concurrent_, needing to wait for multiple
different events at the same time, responding immediately to whatever
event happens first.

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

## Async basics

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
identifiers and modules into our environment that make using Async
more convenient.  Opening `Async.Std` is standard practice for writing
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
When writing a stand-alone program, you need to start the scheduler
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
that is to be run with the value of `d` once it's determined.  The
call to `Deferred.bind` returns a new deferred that becomes determined
when the deferred returned by `f` is determined.  It also implicitly
registers with the scheduler an _Async job_ that is responsible for
running `f` once `d` is determined.

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
    Reader.file_contents filename
    >>= fun text ->
    Writer.save filename ~contents:(String.uppercase text)
  ;;
val uppercase_file : string -> unit Deferred.t = <fun>
```

In the above we've dropped the parentheses around the function on the
right-hand side of the bind, and we didn't add a level of indentation
for the contents of that function.  This is standard practice for
using the bind operator.

Now let's look at another potential use of bind.  In this case, we'll
write a function that counts the number of lines in a file.

```ocaml
# let count_lines filename =
    Reader.file_contents filename
    >>= fun text ->
    List.length (String.split text ~on:'\n')
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
    Reader.file_contents filename
    >>= fun text ->
    return (List.length (String.split text ~on:'\n'))
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
    Reader.file_contents filename
    >>| fun text ->
    List.length (String.split text ~on:'\n')
  ;;
val count_lines : string -> int Deferred.t = <fun>
```

### Ivars and upon

Deferreds are usually built using combinations of `bind`, `map` and
`return`, but sometimes you want to construct a deferred that you can
determine explicitly with user-code.  This is done using an _ivar_,
which is a handle that lets you control precisely when a deferred
becomes determined.

There are three fundamental operations for working with an ivar; you
can create one, using `Ivar.create`, you can read off the deferred
that corresponds to the ivar in question, using `Ivar.read`, and you
can fill an ivar, thus causing that deferred to become determined,
using `Ivar.fill`.  These operations are illustrated below.

```ocaml
# let ivar = Ivar.create ();;
val ivar : '_a Ivar.t = <abstr>
# let def = Ivar.read ivar;;
val def : '_a Ivar.Deferred.t = <abstr>
# Deferred.peek def;;
- : '_a option = None
# Ivar.fill ivar "Hello";;
- : unit = ()
# Deferred.peek def;;
- : string option = Some "Hello"
```

Ivars are something of a low-level feature; operators like map, bind
and return are typically easier to use and think about.  But ivars can
be useful when you want to build complicated synchronization patterns
that can't be constructed naturally otherwise.

As an example, imagine we wanted a way of scheduling a sequence of
actions that would run after a fixed delay.  In addition, we'd like to
guarantee that these delayed actions are executed in the same order
they were scheduled in.  One could imagine building a module for
handling this with the following interface.

```ocaml
# module type Delayer_intf = sig
    type t
    val create : Time.Span.t -> t
    val schedule : t -> (unit -> 'a Deferred.t) -> 'a Deferred.t
  end;;
```

An action is handed to `schedule` in the form of a deferred-returning
thunk (a thunk is a function whose argument is of type `unit`).  A
deferred is handed back to the caller of `schedule` that will
eventually be filled with the contents of the deferred value returned
by the thunk to be scheduled.  We can implement this using an ivar
which we fill after the thunk is called and the deferred it returns
becomes determined.  Instead of using `bind` or `map` for scheduling
these events, we'll use a different operator called `upon`.  Here's
the signature of `upon`:

```ocaml
# upon;;
- : 'a Deferred.t -> ('a -> unit) -> unit = <fun>
```

Like `bind` and `return`, `upon` schedules a callback to be executed
when the deferred it is passed is determined; but unlike those calls,
it doesn't create a new deferred for this callback to fill.

Our delayer implementation is organized around a queue of thunks,
where every call to `schedule` adds a thunk to the queue, and also
schedules a job in the future to grab a thunk off the queue and run
it.  The waiting will be done using the function `after` which takes a
time span and returns a deferred which becomes determined after that
time span elapses.  The role of the ivar here is to take the value
returned by the thunk and use it to fill the deferred returned by the
provided thunk.

```ocaml
# module Delayer : Delayer_intf = struct
    type t = { delay: Time.Span.t;
               jobs: (unit -> unit) Queue.t;
             }

    let create delay =
      { delay; jobs = Queue.create () }

    let schedule t thunk =
      let ivar = Ivar.create () in
      Queue.enqueue t.jobs (fun () ->
        upon (thunk ()) (fun x -> Ivar.fill ivar x));
      upon (after t.delay) (fun () ->
        let job = Queue.dequeue_exn t.jobs in
        job ());
      Ivar.read ivar
  end;;
module Delayer : Delayer_intf
```

This code isn't particularly long, but it is a bit subtle.  This is
typical of code that involves ivars and `upon`, and because of this,
you should stick to the simpler map/bind/return style of working with
deferreds when you can.

## Examples: an echo server

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
call `Reader.read` to get a block of input.  Then, when that's
complete and if a new block was returned, we write that block to the
writer.  Finally, we wait until the writer's buffers are flushed,
waiting on the deferred returned by `Writer.flushed`, at which point
we recur.  If we hit an end-of-file condition, the loop is ended.  The
deferred returned by a call to `copy_blocks` becomes determined only
once the end-of-file condition is hit.

One important aspect of how this is written is that it uses
_pushback_, which is to say that if the writer can't make progress
writing, the reader will stop reading.  If you don't implement
pushback in your servers, then a stopped client can cause your program
to leak memory, since you'll need to allocate space for the data
that's been read in but not yet written out.

Another memory leak you might be concerned with is the chain of
deferreds that is built up as you go through the loop.  After all,
this code constructs an ever-growing chain of binds, each of which
creates a deferred.  In this case, however, all of the deferreds
should become determined precisely when the final deferred in the
chain is determined, in this case, when the `Eof` condition is hit.
Because of this, we could safely replace all of these deferreds with a
single deferred.  Async has logic to do just this, which is
essentially a form of tail-call optimization.

`copy_blocks` provides the logic for handling a client connection, but
we still need to set up a server to receive such connections and
dispatch to `copy_blocks`.  For this, we'll use Async's `Tcp` module,
which has a collection of utilities for creating simple TCP clients
and servers.

```ocaml
(** Starts a TCP server, which listens on the specified port, invoking
    copy_blocks every time a client connects. *)
let run () =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.on_port 8765)
      (fun _addr r w ->
         let buffer = String.create (16 * 1024) in
         copy_blocks buffer r w)
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

<note>
<title>Functions that never return</title>

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

### Improving the echo server

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

The most notable change in this function is the use of Async's `Pipe`.
A `Pipe` is a communication channel that's used for connecting
different parts of your program.  You can think of it as a
consumer/producer queue that uses deferreds for communicating when the
pipe is ready to be read from or written to.  Our use of pipes is
fairly minimal here, but they are an important part of Async, so it's
worth discussing them in some detail.

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
throughout the process, so that if the writer gets blocked, the
writer's pipe will stop pulling data from the reader's pipe, which
will prevent the reader from reading in more data.

Importantly, the deferred returned by `Pipe.transfer` becomes
determined once the reader has been closed and the last element is
transferred from the reader to the writer.  Once that deferred becomes
determined, the server will shut down that client connection.  So,
when a client disconnects, the rest of the shutdown happens
transparently.

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

## Example: searching definitions with DuckDuckGo

DuckDuckGo is a search engine with a freely available search
interface.  In this section, we'll use Async to write a small
command-line utility for querying DuckDuckGo to extract definitions
for a collection of terms.

Our code is going to rely on a number of other libraries, all of which
can be installed using OPAM.  Refer to [xref](#installation) if you
need help on the installation.  Here's the list of libraries we'll
need.

- `textwrap`, a library for wrapping long lines.  We'll use this for
  printing out our results.
- `uri`, a library for handling URI's, or "Uniform Resource
  Identifiers", of which HTTP URL's are an example.
- `yojson`, a JSON parsing library that was described in
  [xref](#handling-json-data)
- `cohttp`, a library for creating HTTP clients and servers.  We need
  Async support, which comes with the `cohttp.async` package.

Now let's dive into the implementation.

### URI handling

You're probably familiar with HTTP URLs, which identify endpoints
across the World Wide Web.  These are actually part of a more general
family known as Uniform Resource Identifiers (URIs). The full URI
specification is defined in
[RFC3986](http://tools.ietf.org/html/rfc3986), and is rather
complicated.  Luckily, the `uri` library provides a strongly-typed
interface which takes care of much of the hassle.

We'll need a function for generating the URI's that we're going to use
to query the DuckDuckGo servers.

```ocaml
(* file: search.ml *)
open Core.Std
open Async.Std

(* Generate a DuckDuckGo search URI from a query string *)
let query_uri query =
  let base_uri = Uri.of_string "http://api.duckduckgo.com/?format=json" in
  Uri.add_query_param base_uri ("q", [query])
```

A `Uri.t` is constructed from the `Uri.of_string` function, and a
query parameter `q` is added with the desired search query.  The
library takes care of encoding the URI correctly when outputting it in
the network protocol.

### Parsing JSON strings

The HTTP response from DuckDuckGo is in JSON, a common (and thankfully
simple) format that is specified in
[RFC4627](http://www.ietf.org/rfc/rfc4627.txt).  We'll parse the JSON
data using the Yojson library, which we already introduced in
[xref](#handling-json-data).

We expect the response from DuckDuckGo to come across as a JSON
record, which is represented by the `Assoc` tag in Yojson's JSON
variant.  We expect the definition itself to come across under either
the key "Abstract" or "Definition", and so the code below looks under
both keys, returning the first one for which a non-empty value is
defined.

```ocaml
(* Extract the "Definition" or "Abstract" field from the DuckDuckGo results *)
let get_definition_from_json json =
  match Yojson.Safe.from_string json with
  | `Assoc kv_list ->
    let find key =
      begin match List.Assoc.find kv_list key with
      | None | Some (`String "") -> None
      | Some s -> Some (Yojson.Safe.to_string s)
      end
    in
    begin match find "Abstract" with
    | Some _ as x -> x
    | None -> find "Definition"
    end
  | _ -> None
```

### Executing an HTTP client query

Now let's look at the code for dispatching the search queries over
HTTP, using the Cohttp library.

```ocaml
(* Execute the DuckDuckGo search *)
let get_definition word =
  Cohttp_async.Client.get (query_uri word)
  >>= fun (_, body) ->
  Pipe.to_list body
  >>| fun strings ->
  (word, get_definition_from_json (String.concat strings))
```

To better understand what's going on, it's useful to look at the type
for `Cohttp_async.Client.get`, which we can do in utop.

```ocaml
# #require "cohttp.async";;
# Cohttp_async.Client.get;;
- : ?interrupt:unit Deferred.t ->
    ?headers:Cohttp.Header.t ->
    Uri.t -> (Cohttp.Response.r * Cohttp_async.body) Deferred.t
= <fun>
```

The `get` call takes as a required argument a URI, and returns a
deferred value containing a `Cohttp.Response.t` (which we ignore) and
a pipe reader to which the body of the request will be written to as
it is received.

In this case, the HTTP body probably isn't very large, so we call
`Pipe.to_list` to collect the strings from the pipe as a single
deferred list of strings.  We then join those strings using
`String.concat` and pass the result through our parsing function.

Running a single search isn't that interesting from a concurrency
perspective, so let's write code for dispatching multiple searches in
parallel.  First, we need code for formatting and printing out the
search result.

```ocaml
(* Print out a word/definition pair *)
let print_result (word,definition) =
  printf "%s\n%s\n\n%s\n\n"
    word
    (String.init (String.length word) ~f:(fun _ -> '-'))
    (match definition with
    | None -> "No definition found"
    | Some def ->
      String.concat ~sep:"\n"
        (Wrapper.wrap (Wrapper.make 70) def))
```

We use the `Wrapper` module from the `textwrap` package to do the
line-wrapping.  It may not be obvious that this routine is using
Async, but it does: the version of `printf` that's called here is
actually Async's specialized `printf` that goes through the Async
scheduler rather than printing directly.  The original definition of
`printf` is shadowed by this new one when you open `Async.Std`.  An
important side effect of this is that if you write an Async program
and forget to start the scheduler, calls like `printf` won't actually
generate any output!

The next function dispatches the searches in parallel, waits for the
results, and then prints.

```ocaml
(* Run many searches in parallel, printing out the results after they're all
   done. *)
let search_and_print words =
  Deferred.all (List.map words ~f:get_definition)
  >>| fun results ->
  List.iter results ~f:print_result
```

We used `List.map` to call `get_definition` on each word, and
`Deferred.all` to wait for all the results.  Here's the type of
`Deferred.all`:

```ocaml
# Deferred.all;;
- : 'a Deferred.t list -> 'a list Deferred.t = <fun>
```

Note that the list returned by `Deferred.all` reflects the order of
the deferreds passed to it.  As such, the definitions will be printed
out in the same order that the search words are passed in, no matter
what orders the queries return in.  We could rewrite this code to
print out the results as they're received (and thus potentially out of
order) as follows.

```ocaml
(* Run many searches in parallel, printing out the results as you go *)
let search_and_print words =
  Deferred.all_unit (List.map words ~f:(fun word ->
    get_definition word >>| print_result))
```

The difference is that we both dispatch the query and print out the
result in the closure passed to `map`, rather than waiting for all of
the results to get back and then printing them out together.  We use
`Deferred.all_unit`, which takes a list of `unit` deferreds and
returns a single `unit` deferred that becomes determined when every
deferred on the input list is determined.  We can see the type of this
function in utop.

```ocaml
# Deferred.all_unit;;
- : unit Deferred.t list -> unit Deferred.t = <fun>
```

Finally, we create a command line interface using
`Command.async_basic`.

```ocaml
let () =
  Command.async_basic
    ~summary:"Retrieve definitions from duckduckgo search engine"
    Command.Spec.(
      empty
      +> anon (sequence ("word" %: string))
    )
    (fun words () -> search_and_print words)
  |> Command.run
```

And that's all we need to create a simple but usable definition
searcher.

```
$ ./search.native "Concurrent Programming" "OCaml"
Concurrent Programming
----------------------

"Concurrent computing is a form of computing in which programs are
designed as collections of interacting computational processes that
may be executed in parallel."

OCaml
-----

"OCaml, originally known as Objective Caml, is the main implementation
of the Caml programming language, created by Xavier Leroy, Jérôme
Vouillon, Damien Doligez, Didier Rémy and others in 1996."
```

## Exception handling

When programming with external resources, errors are everywhere:
everything from a flaky server to a network outage to exhausting of
local resources can lead to a runtime error.  When programming in
OCaml, some of these errors will show up explicitly in a function's
return type, and some of them will show up as exceptions.  We covered
exception handling in OCaml in [xref](#exceptions), but as we'll see,
exception handling in a concurrent program presents some new
challenges.

Let's get a better sense of how exceptions work in Async by creating
an asynchronous computation that (sometimes) fails with an exception.
The function `maybe_raise` below blocks for half a second, and then
either throws an exception or returns unit, alternating between the
two behaviors on subsequent calls.  

```ocaml
# let maybe_raise =
    let should_fail = ref false in
    fun () ->
      let will_fail = !should_fail in
      should_fail := not will_fail;
      after (Time.Span.of_sec 0.5)
      >>= fun () ->
      if will_fail then raise Exit else return ()
 ;;
val maybe_raise : Core.Span.t -> unit Deferred.t = <fun>
# maybe_raise ();;
- : unit = ()
# maybe_raise ();;
Exception:
(lib/monitor.ml.Error_
 ((exn Exit) (backtrace (""))
  (monitor
   (((name block_on_async) (here ()) (id 5) (has_seen_error true)
     (someone_is_listening true) (kill_index 0))
    ((name main) (here ()) (id 1) (has_seen_error false)
     (someone_is_listening false) (kill_index 0)))))).
```

In utop, the exception thrown by `maybe_raise ()` terminates the
evaluation of just that expression, but in a stand-alone program, an
uncaught exception would bring down the entire process.

So, how could we capture and handle such an exception?  You might try
to do this using OCaml's built-in `try/with` statement, but as you can
see below, that doesn't quite do the trick.

```ocaml
# let handle_error () =
    try
      maybe_raise ()
      >>| fun () -> "success"
    with _ -> return "failure"
  ;;
val handle_error : unit -> string Deferred.t = <fun>
# handle_error ();;
- : string = "success"
# handle_error ();;
Exception:
(lib/monitor.ml.Error_
 ((exn Exit) (backtrace (""))
  (monitor
   (((name block_on_async) (here ()) (id 58) (has_seen_error true)
     (someone_is_listening true) (kill_index 0))
    ((name main) (here ()) (id 1) (has_seen_error false)
     (someone_is_listening false) (kill_index 0)))))).
```

This didn't work because `try/with` only captures exceptions that are
thrown in the code directly executed within it, while `maybe_raise`
schedules an Async job to run in the future, and it's that job that
throws an exception.  

We can capture this kind of asynchronous error use the `try_with`
function provided by Async:

```ocaml
# let handle_error () =
    try_with (fun () -> maybe_raise ())
    >>| function
    | Ok ()   -> "success"
    | Error _ -> "failure"
  ;;
# handle_error ();;
- : string = "success"
# handle_error ();;
  - : string = "failure"
```

`try_with f` takes as its argument a deferred-returning thunk `f`, and
returns a deferred that becomes determined either as `Ok` of whatever
`f` returned, or `Error exn` if `f` threw an exception before its
return value became determined.

### Monitors

`try_with` is a a great way of handling exceptions in Async, but it's
not the whole story.  All of Async's exception-handling mechanisms,
`try_with` included, are built on top of Async's system of _monitors_,
which are inspired by the error-handling mechanism in Erlang of the
same name.  Monitors are fairly low-level and are only occasionally
used directly, but it's nonetheless worth understanding how they work.

In Async, a monitor is a context that determines what to do when there
is an unhandled exception.  Every Async job runs within the context of
some monitor, which, when the job is running, is referred to as the
current monitor.  When a new Async job is scheduled, say, using `bind`
or `map`, it inherits the current monitor of the job that spawned it.

Monitors are arranged in a tree -- when a new monitor is created (say,
using `Monitor.create`) it is a child of the current monitor.  You can
explicitly run jobs within a monitor using `within`, which takes a
thunk that returns a non-deferred value, or `within'`, which takes a
thunk that returns a deferred.  Here's an example.

```ocaml
# let blow_up () =
    let monitor = Monitor.create ~name:"blow up monitor" () in
    within' ~monitor maybe_raise
  ;;
# blow_up ();;
- : unit = ()
# blow_up ();;
Exception:
(lib/monitor.ml.Error_
 ((exn Exit) (backtrace (""))
  (monitor
   (((name "blow up monitor") (here ()) (id 73) (has_seen_error true)
     (someone_is_listening false) (kill_index 0))
    ((name block_on_async) (here ()) (id 72) (has_seen_error false)
     (someone_is_listening true) (kill_index 0))
    ((name main) (here ()) (id 1) (has_seen_error false)
     (someone_is_listening false) (kill_index 0)))))).
```

In addition to the ordinary stack-trace, the exception displays the
trace of monitors through which the exception traveled, starting at
the one we created, called "blow up monitor".  The other monitors you
see come from utop's special handling of deferreds.

Monitors can do more than just augment the error-trace of an
exception.  You can also use a monitor to explicitly handle errors
delivered to that monitor.  The `Monitor.errors` call is a
particularly important one.  It detaches the monitor from its parent,
handing back the stream of errors that would otherwise have been
delivered to the parent monitor.  This allows one to do custom
handling of errors, which may include re-raising errors to the parent.
Here is a very simple example of function that captures and ignores
errors in the processes it spawns.

```ocaml
# let swallow_error () =
    let monitor = Monitor.create () in
    Stream.iter (Monitor.errors monitor) ~f:(fun _exn ->
      printf "an error happened\n");
    within' ~monitor (fun () ->
      after (Time.Span.of_sec 0.5) >>= fun () -> failwith "Kaboom!")
  ;;
val swallow_error : unit -> 'a Deferred.t = <fun>
# swallow_error ();;
an error happened
```

The message "an error happened" is printed out, but the deferred
returned by `swallow_error` is never determined.  This makes sense,
since the calculation never actually completes, so there's no value to
return.  You can break out of this in utop by hitting `Control-C`.

Here's an example of a monitor which passes some exceptions through to
the parent, and handles others.  Exceptions are sent to the parent
using `Monitor.send_exn`, with `Monitor.current` being called to find
the current monitor, which is the parent of the newly created monitor.

```ocaml
# exception Ignore_me;;
exception Ignore_me
# let swallow_some_errors exn_to_raise =
    let child_monitor  = Monitor.create  () in
    let parent_monitor = Monitor.current () in
    Stream.iter (Monitor.errors child_monitor) ~f:(fun error ->
      match Monitor.extract_exn error with
      | Ignore_me -> printf "ignoring exn\n"
      | _ -> Monitor.send_exn parent_monitor error);
    within' ~monitor:child_monitor (fun () ->
       after (Time.Span.of_sec 0.5)
       >>= fun () -> raise exn_to_raise)
  ;;
val swallow_some_errors : exn -> 'a Deferred.t = <fun>
```

Note that we use `Monitor.extract_exn` to grab the underlying
exception that was thrown.  Async wraps exceptions it catches with
extra information, including the monitor trace, so you need to grab
the underlying exception to match on it.

If we pass in an exception other than `Ignore_me`, like, say, the
built-in exception `Not_found`, then the exception will be passed to
the parent monitor and delivered as usual.

```ocaml
# swallow_some_errors Not_found;;
Exception:
(lib/monitor.ml.Error_
 ((exn Not_found) (backtrace (""))
  (monitor
   (((name (id 3)) (here ()) (id 3) (has_seen_error true)
     (someone_is_listening true) (kill_index 0))
    ((name block_on_async) (here ()) (id 2) (has_seen_error true)
     (someone_is_listening true) (kill_index 0))
    ((name main) (here ()) (id 1) (has_seen_error false)
     (someone_is_listening false) (kill_index 0)))))).
```

If instead we use `Ignore_me`, the exception will be ignored, and we
again see that the deferred never returns, but the exception was
caught and ignored.

```ocaml
# swallow_some_errors Ignore_me;;
ignoring exn
```

In practice, you should rarely use monitors directly, instead using
functions like `try_with` and `Monitor.protect` that are built on top
of monitors.  One example of a library that uses monitors directly is
`Tcp.Server.create`, which tracks both exceptions thrown by the logic
that handles the network connection and by the callback for responding
to an individual request, in either case responding to an exception by
closing the connection.  It is for building this kind of custom error
handling that monitors can be helpful.

### Example: Handling exceptions with DuckDuckGo

Let's now go back and improve the exception handling of our DuckDuckGo
client.  In particular, we'll change it so that any individual queries
that fail are reported as such, without preventing other queries from
succeeding.

The search code as it is fails rarely, so let's make a change that
allows us to trigger failures more predictably.  We'll do this by
making it possible to distribute the requests over multiple servers.
Then, we'll handle the errors that occur when one of those servers is
misspecified.

First we'll need to change `query_uri` to take an argument specifying
the server to connect to, as follows.

```ocaml
(* Generate a DuckDuckGo search URI from a query string *)
let query_uri ~server query =
  let base_uri =
    Uri.of_string (String.concat ["http://";server;"/?format=json"])
  in
  Uri.add_query_param base_uri ("q", [query])
```

and then making the appropriate changes to get the list of servers on
the command-line, and to distribute the search queries round-robin
over the list of servers.  Now, let's see what happens if we rebuild
the application and run it giving it a list of servers, some of which
won't respond to the query.

```
$ ./search_with_configurable_server.native \
     -servers localhost,api.duckduckgo.com \
     "Concurrent Programming" OCaml
("unhandled exception"
 ((lib/monitor.ml.Error_
   ((exn (Unix.Unix_error "Connection refused" connect 127.0.0.1:80))
    (backtrace
     ("Raised by primitive operation at file \"lib/unix_syscalls.ml\", line 793, characters 12-69"
      "Called from file \"lib/deferred.ml\", line 24, characters 62-65"
      "Called from file \"lib/scheduler.ml\", line 120, characters 6-17"
      "Called from file \"lib/jobs.ml\", line 73, characters 8-13" ""))
    (monitor
     (((name Tcp.close_sock_on_error) (here ()) (id 3) (has_seen_error true)
       (someone_is_listening true) (kill_index 0))
      ((name main) (here ()) (id 1) (has_seen_error true)
       (someone_is_listening false) (kill_index 0))))))
  (Pid 1352)))
```

As you can see, we got a "Connection refused" failure which ends the
entire program, even though one of the two queries would have gone
through successfully. We can handle the failures of individual
connections separately by using the `try_with` function within each
call to `get_definition`, as follows.

```ocaml
(* Execute the DuckDuckGo search *)
let get_definition ~server word =
  try_with (fun () ->
    Cohttp_async.Client.get (query_uri ~server word)
    >>= fun  (_, body) ->
    Pipe.to_list body
    >>| fun strings ->
    (word, get_definition_from_json (String.concat strings)))
  >>| function
  | Ok (word,result) -> (word, Ok result)
  | Error _          -> (word, Error "Unexpected failure")
```

Here, we use `try_with` to capture the exception, which we then use
map (the `>>|` operator) to convert the error into the form we want: a
pair whose first element is the word being searched for, and the
second element is the (possibly erroneous) result.

Now we just need to change the code for `print_result` so that it can
handle the new type.

```ocaml
(* Print out a word/definition pair *)
let print_result (word,definition) =
  printf "%s\n%s\n\n%s\n\n"
    word
    (String.init (String.length word) ~f:(fun _ -> '-'))
    (match definition with
     | Error s -> "DuckDuckGo query failed: " ^ s
     | Ok None -> "No definition found"
     | Ok (Some def) ->
       String.concat ~sep:"\n"
         (Wrapper.wrap (Wrapper.make 70) def))
```

Now, if we run that same query, we'll get individualized handling of
the connection failures:

```
$ ./search_with_error_handling.native \
     -servers localhost,api.duckduckgo.com \
     "Concurrent Programming" OCaml
Concurrent Programming
----------------------

DuckDuckGo query failed unexpectedly

OCaml
-----

"OCaml, originally known as Objective Caml, is the main implementation
of the Caml programming language, created by Xavier Leroy, Jérôme
Vouillon, Damien Doligez, Didier Rémy and others in 1996."
```

Now, only the query that went to `localhost` failed.

Note that in this code, we're relying on the fact that
`Cohttp_async.Client.get` will clean up after itself after an
exception, in particular by closing its file descriptors.  If you need
to implement such functionality directly, you may want to use the
`Monitor.protect` call, which is analogous to the `protect` call
described in [xref](#cleaning-up-in-the-presence-of-exceptions).

## Timeouts, Cancellation and Choices

In a concurrent program, one often needs to combine results from
multiple distinct concurrent sub-computations going on in the same
program.  We already saw this in our DuckDuckGo example, where we used
`Deferred.all` and `Deferred.all_unit` to wait for a list of deferreds
to become determined.  Another useful primitive is `Deferred.both`,
which lets you wait until two deferreds of different types have
returned, returning both values as a tuple.  Here, we use the function
`sec`, which is shorthand for creating a time-span equal to a given
number of seconds.

```ocaml
# let string_and_float = Deferred.both
   (after (sec 0.5)  >>| fun () -> "A")
   (after (sec 0.25) >>| fun () -> 32.33);;
val string_and_float : (string * float) Deferred.t = <abstr>
# string_and_float;;
- : string * float = ("A", 32.33)
```

Sometimes, however, we want to wait only for the first of multiple
events to occur.  This happens particularly often when dealing with
timeouts.  In that case, we can use the call `Deferred.any`, which,
given a list of deferreds, returns a single deferred that will become
determined once any of the values on the list is determined.

```ocaml
# Deferred.any [ (after (sec 0.5) >>| fun () -> "half a second")
               ; (after (sec 10.) >>| fun () -> "ten seconds") ] ;;
- : string = "half a second"
```

Let's use this to add timeouts to our DuckDuckGo searches.  We'll do
this by writing a wrapper for `get_definition` that takes a timeout
(in the form of a `Time.Span.t`) as an argument, and returns either
the definition, or, if that takes too long, the timeout.

```ocaml
let get_definition_with_timeout ~server ~timeout word =
  Deferred.any
    [ (after timeout >>| fun () -> (word,Error "Timed out"))
    ; (get_definition ~server word
       >>| fun (word,result) ->
       let result' = match result with
         | Ok _ as x -> x
         | Error _ -> Error "Unexpected failure"
       in
       (word,result')
      )
    ]
```

We use `>>|` above to transform the deferred values we're waiting for
so that `Deferred.any` can choose between values of the same type.

A problem with this code is that the HTTP query kicked off by
`get_definition` is not actually shut down when the timeout fires.  As
such, `get_definition_with_timeout` essentially leaks an open
connection.  Happily, Cohttp does provide a way of shutting down a
client.  You can pass a deferred under the label `interrupt` to
`Cohttp_async.Client.get`.  Once `interrupt` is determined, the client
connection will terminated and the corresponding connections closed.

The following code shows how you can change `get_definition` and
`get_definition_with_timeout` to cancel the `get` call if the timeout
expires.  

```ocaml
(* Execute the DuckDuckGo search *)
let get_definition ~server ~interrupt word =
  try_with (fun () ->
    Cohttp_async.Client.get ~interrupt (query_uri ~server word)
    >>= fun  (_, body) ->
    Pipe.to_list body
    >>| fun strings ->
    (word, get_definition_from_json (String.concat strings)))
  >>| function
  | Ok (word,result) -> (word, Ok result)
  | Error exn        -> (word, Error exn)
```

Next, we'll modify `get_definition_with_timeout` to create a deferred
to pass in to `get_definition` which will become determined when our
timeout expires.

```ocaml
let get_definition_with_timeout ~server ~timeout word =
  get_definition ~server ~interrupt:(after timeout) word
  >>| fun (word,result) ->
  let result' = match result with
    | Ok _ as x -> x
    | Error _ -> Error "Unexpected failure"
  in
  (word,result')
```

This will work, and will cause the connection to shut-down cleanly
when we time out; but our code no longer explicitly knows whether or
not the timeout has kicked in.  In particular, the error message on a
timeout will now be `Unexpected failure` rather than `Timed out`,
which it was in our previous implementation.  This is a minor issue in
this case, but if we wanted to have special behavior in the case of a
timeout, it would be a more serious issue.

We can get more precise handling of timeouts using Async's `choose`
operator, which lets you pick between a collection of different
deferreds, reacting to exactly one of them.  Each deferred is
combined, using the function `choice`, with a function that is called
if and only if that is the chosen deferred.   Here's the type
signature of `choice` and `choose`:

```ocaml
# choice;;
- : 'a Deferred.t -> ('a -> 'b) -> 'b Deferred.choice = <fun>
# choose;;
- : 'a Deferred.choice list -> 'a Deferred.t = <fun>
```

`choose` provides no guarantee that the `choice` built around the
first deferred to become determined will in fact be chosen.  But
`choose` does guarantee that only one `choice` will be chosen, and
only the chosen `choice` will execute the attached closure.

In the following, we use `choose` to ensure that the `interrupt`
deferred becomes determined if and only if the timeout-deferred is
chosen.  Here's the code.

```ocaml
let get_definition_with_timeout ~server ~timeout word =
  let interrupt = Ivar.create () in
  choose
    [ choice (after timeout) (fun () ->
       Ivar.fill interrupt ();
       (word,Error "Timed out"))
    ; choice (get_definition ~server ~interrupt:(Ivar.read interrupt) word)
        (fun (word,result) ->
           let result' = match result with
             | Ok _ as x -> x
             | Error _ -> Error "Unexpected failure"
           in
           (word,result')
        )
    ]
```

Now, if we run this with a suitably small timeout, we'll see that some
queries succeed and some fail, and the timeouts are reported as such.

```
$ ./search_with_timeout_no_leak.native "concurrent programming" ocaml -timeout 0.1s
concurrent programming
----------------------

DuckDuckGo query failed: Timed out

ocaml
-----

"OCaml or Objective Caml, is the main implementation of the Caml
programming language, created by Xavier Leroy, Jérôme Vouillon,
Damien Doligez, Didier Rémy and others in 1996."
```

