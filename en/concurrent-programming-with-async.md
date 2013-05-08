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
throughout the process, so that if the writer gets blocked, the the
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
  [xref](#parsing-json-with-yojson)
- `cohttp`, a library for creating HTTP clients and servers.  We need
  Async support, which comes with the `cohttp.async` package.

Now let's dive into the implementation.

### URI handling

You're probably familiar with HTTP URLs, which identify endpoints
across the World Wide Web.  These are actually part of a more general
family known as Uniform Resource Identifiers (URIs). The full URI
specification is defined in
[RFC3986](http://tools.ietf.org/html/rfc3986), and is rather
complicated.  Luckily, the `ocaml-uri` library provides a
strongly-typed interface which takes care of much of the hassle.

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

Note that the URI manipulation functions are all pure functions which
return a new URI value, and never modify the input.  This makes it
easier to pass around URI values through your application stack
without fear of modification.

### Parsing JSON strings

The HTTP response from DuckDuckGo is in JSON, a common (and thankfully
simple) format that is specified in
[RFC4627](http://www.ietf.org/rfc/rfc4627.txt).  There are quite a few
JSON parsers available for OCaml, and we've picked
[`Yojson`](http://mjambon.com/yojson.html) for this example.

There are a few non-standard extensions to JSON, so Yojson exposes
them as the `Basic` and `Safe` sub-modules.  It doesn't really matter
which one we pick for this simple example, so we'll go with `Safe`.

The input `string` is parsed using `Yojson.Safe.from_string` into an
OCaml data type. The JSON values are represented using polymorphic
variants, and can thus be pattern matched more easily once they have
been parsed by Yojson.

```ocaml
type json = [
  | `Assoc of (string * json) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of json list
  | `Null
  | `String of string
  | `Tuple of json list
]
```ocaml

We're expecting the DuckDuckGo response to be a record, with an
optional `Description` field being one of the keys in the record.  The
`get_definition_from_json` does a pattern match on this, and returns
an optional string if a definition is found within the result.  The
description field sometimes contains an empty string, which we
explicitly recognize as a null response in our code.

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

Note that we check two different fields, `Abstract` and `Definition`,
since DuckDuckGo sometimes puts the definition in one location and
sometimes in the other.

### Executing an HTTP client query

Now that we've written those utility functions, let's look at the code
for dispatching the search queries over HTTP.

```ocaml
(* Execute the DuckDuckGo search *)
let get_definition word =
  Cohttp_async.Client.call `GET (query_uri word)
  >>= function
  | None | Some (_, None) -> return (word, None)
  | Some (_, Some body) ->
    Pipe.to_list body >>| fun strings ->
    (word, get_definition_from_json (String.concat strings))
```

The `Cohttp.Client` module executes the HTTP call, and returns a
deferred status and response.  To better understand what's going on,
it's useful to look at the type for `Cohttp_async.Client.call`, which
we can do in utop.

```ocaml
# #require "cohttp.async";;
# Cohttp_async.Client.call;;
- : ?headers:Cohttp.Header.t ->
    ?body:string Pipe.Reader.t ->
    Cohttp.Code.meth ->
    Uri.t ->
    (Cohttp_async.Response.t * string Pipe.Reader.t option) option Deferred.t
= <fun>
```

`call` has two required arguments; the method (` ``GET`, in this
case), and the URI.  An optional deferred value is returned,
containing a `Cohttp_async.Response.t` (which we ignore) and an
optional pipe reader which will receive the stream of strings which
the Cohttp client writes the incoming data to.

In this case, the HTTP body probably isn't very large, so we just call
`Pipe.to_list` to collect all the output of the pipe and returns those
elements as a deferred list.  We then join that list into a string
using `String.concat` and pass it through our JSON parser to extract
the definitions.

Running a single search isn't that interesting from a concurrency
perspective, so let's write code for dispatching multiple searches in
parallel.  First, we need code for printing out a result.

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

We use the `Wrapper` module that comes from the `textwrap` package to
do the line-wrapping.  `print_result` doesn't look like it uses Async,
but it does: the version of `printf` that's called here is actually
Async's wrapping of `printf` that goes through the Async scheduler
rather than immediately printing to standard out.  The shadowing of
the original definition of `printf` is done when you open `Async.Std`.

Next we need to dispatch the searches in parallel, wait for the
results, and print out what we find.  

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
out in the same order that the search wrods are passed in, no matter
what orders the queries return in.  We could rewrite this code to
print out the results as they're received and thus potentially out of
order:

```ocaml
(* Run many searches in parallel, printing out the results as you go *)
let search_and_print words =
  Deferred.all_unit (List.map words ~f:(fun word ->
    get_definition word >>| print_result))
```

The difference is that we both dispatch the query and print out the
result in the closure passed to `map`, rather than waiting for all of
the results to get back and then printing them out together.  We use
`Deferred.all_unit` to wait for the results, which has this signature:

```ocaml
# Deferred.all_unit;;
- : unit Deferred.t list -> unit Deferred.t = <fun>
```

Finally, we need to create a command line interface, again using
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

And once we build this, we'll have a simple but usable definition
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

When programming with external resources, errors are everywhere: your
connection might be refused, you can run out of file descriptors, your
connection might get dropped by the server on the other side, and so
on.  Some of these errors show up explicitly in the return type of a
function, but errors are pervasive enough in concurrent programming
that representing them explicitly everywhere is of limited usefulness.
Instead, much of the time, such errors come through as exceptions.  In
this section we'll describe Async's mechanisms for handling
exceptions.

Let's get a better sense of how exceptions work in Async by creating a
computation that sometimes fails with an exception.  The following
example blocks for a while, (using the `after` function, which takes a
time-span and returns a deferred that will become determined after
that time span is elapsed), and then either throws an exception or
returns unit.  More precisely, it alternates between failing and
succeeding, as you can see below.

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

In utop, an exception just terminates the expression you're
evaluating, but in a stand-alone program, an uncaught exception would
bring down the entire process.

Note that OCaml's built-in `try/with` statement isn't enough to catch
errors from such process, as you can see below.

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

This didn't work because the `try/with` only captures exceptions that
are thrown in the code directly executed within it; but `maybe_raise`
schedules a job to run in the future, and it's that job that throws an
exception.  Such exceptions can be caught using the `try_with`
function provided by Async, as shown below.

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

Essentially, ``try_with f` takes as its argument a deferred-returning
thunk `f` (a thunk is a function whose argument is unit), and returns
a deferred that becomes determined either as `Ok` of whatever `f`
returned, or `Error exn` if `f` threw an exception before its return
value became determined.

In order to better understand how this actually works, you need to
understand Async's system of monitors, which are at the heart of its
exception handling strategy.

### Monitors

A monitor is a context that determines what to do when there is an
unhandled exception.  Every Async computation runs within the context
of some monitor, which, when the computation is running, is referred
to as the current monitor.  When a new async job is scheduled, say,
using `bind` or `map`, it inherits the current monitor of the job that
spawned it.

Monitors are arranged in a tree -- when a new monitor is created (say,
using `Monitor.create`) it is a child of the current monitor.  You can
then run jobs within this new monitor using the `within` call (which
expects a function that returns unit) or `within'` (which expects a
function that returns a deferred).  Here's an example.

```ocaml
# let blow_up () =
    let monitor = Monitor.create ~name:"blow up monitor" () in
    within' ~monitor (fun () ->
      after (Time.Span.of_sec 0.5) >>= fun () -> failwith "Kaboom!")
  ;;
# blow_up ();;
Exception:
(lib/monitor.ml.Error_
 ((exn (Failure Kaboom!))
  (backtrace
   ("Raised at file \"pervasives.ml\", line 20, characters 22-33" ""))
  (monitor
   (((name "blow up monitor") (here ()) (id 3) (has_seen_error true)
     (someone_is_listening false) (kill_index 0))
    ((name block_on_async) (here ()) (id 2) (has_seen_error false)
     (someone_is_listening true) (kill_index 0))
    ((name main) (here ()) (id 1) (has_seen_error false)
     (someone_is_listening false) (kill_index 0)))))).
```

As you can see, in addition to the ordinary stack-trace, the exception
displays the trace of monitors through which the exception traveled,
starting at the one we created, called "blow up monitor".  In this
case, the other monitors come from utop's special handling of deferred
computations.

But monitors can do more than just augment the error-trace of an
exception.  You can also explicitly handle the errors in a monitor.
In particular, the `Monitor.errors` call returns the stream of errors
handled by that monitor.  Here, for example, is a function that
captures and ignores errors in the processes it spawns.

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

If you run this, you'll see that the error message gets printed out,
but `swallow_error`, but utop blocks out because the deferred returned
by `swallow_error` never gets determined, which makes sense, because
the calculation never actually completes, so there's no value to
return.  You can break out of this by hitting `Control-C`.

All of this should highlight the fact that monitors are a fairly
low-level error-handling tool.  They're powerful, and give you full
control of how errors are handled, but you're typically better off
using higher-level abstractions built on top of monitors like
`try_with`.


### Example: Handling exceptions in our definition search

Let's consider error handling for the definition search code that we
wrote earlier in the chapter.  In particular, lets catch errors so
that we'll report any queries that fail, but the failure of one query
won't interfere with a different one.

The search code as it is fails rarely, so let's make make a change
that can cause it to fail more predictably, by making the server we
connecto to configurable.  Then, we'll handle the errors that occur
when you specify the wrong host.

We make the choice of server configurable by changing `query_uri` to
take an argument specifying the server to connect to:

```ocaml
(* Generate a DuckDuckGo search URI from a query string *)
let query_uri ~server query =
  let base_uri =
    Uri.of_string (String.concat ["http://";server;"/?format=json"])
  in
  Uri.add_query_param base_uri ("q", [query])
```

and then making the appropriate changes to pass through the [~server]
argument through the other function calls as necessary.  Then we just
need to add a flag so we can specify the server at the command line:

```ocaml
let () =
  Command.async_basic
    ~summary:"Retrieve definitions from duckduckgo search engine"
    Command.Spec.(
      empty
      +> anon (sequence ("word" %: string))
      +> flag "-server" (optional_with_default "api.duckduckgo.com" string)
           ~doc:" Specify server to connect to"
    )
    (fun words server () -> search_and_print ~server words)
  |> Command.run
```

Now, if we rebuild the application and run it as is, we'll see the
following error:

```
$ ./search_with_configurable_server.native -server localhost \
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

As you can see, we got a "Connection refused" failure, which was
passed up to the toplevel monitor, which ended the program.  We can
handle the failures of individual connections separately by using the
`try_with` function within each call to `get_definition`, as follows.

```ocaml
(* Execute the DuckDuckGo search *)
let get_definition ~server word =
  try_with (fun () ->
    Cohttp_async.Client.call `GET (query_uri ~server word)
    >>= function
    | None | Some (_, None) -> return (word, None)
    | Some (_, Some body) ->
      Pipe.to_list body >>| fun strings ->
      (word, get_definition_from_json (String.concat strings)))
  >>| function
  | Ok (word,result) -> (word, Ok result)
  | Error exn        -> (word, Error exn)
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
     | Error _ -> "DuckDuckGo query failed unexpectedly"
     | Ok None -> "No definition found"
     | Ok (Some def) ->
       String.concat ~sep:"\n"
         (Wrapper.wrap (Wrapper.make 70) def))
```

Now, if we run that same query, we'll get individualized handling of
the connection failures:

```
$ ./search_with_error_handling.native -server localhost \
     "Concurrent Programming" OCaml
> Concurrent Programming
----------------------

DuckDuckGo query failed unexpectedly

OCaml
-----

DuckDuckGo query failed unexpectedly

```

Note that in the above code we're relying on the call to
`Cohttp_async.Client.call` to clean up after itself.  In particular,
it needs to make sure to close whatever file descriptors have been
opened.  There are other useful error-handling utlities in Async that
are helpful here, notably the `Monitor.protect` call, which is
analogous to the `protect` call described in
[xref](#cleaning-up-in-the-presence-of-exceptions).  The Async API
docs, for `Monitor` in particular, are a good source for learning what
exception-handling calls exist.

## Timeouts, Cancellation and Choices

One common kind of operation in a concurrent programming is the need
to combine results from multiple distinct concurrent processes going
on in the same program.  We already saw this in our web-search
example, using `Deferred.all` and `Deferred.all_unit` to wait for all
of a list of deferreds to become determined.  Another useful primitive
is `Deferred.both`, which lets you wait until two deferreds of
different types have returned, returning both values as a tuple:

```ocaml
# let both = Deferred.both
   (after (sec 0.5)  >>| fun () -> "A")
   (after (sec 0.25) >>| fun () -> 32.33)
 ;; 
val both : (string * float) Deferred.t = <abstr>
# both;;
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

We can see how this would work in action by extending our definition
search tool to timeout on any queries that take too long to satisfy.
We'll do this by writing a wrapper for `get_definition` that takes a
timeout (in the form of a `Time.Span.t`) as an argument, and returns
either the definition, or the timeout, whichever finished first.

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

One problem with the above code is that the HTTP query kicked off by
`get_definition` is not actually shut down when the timeout fires.  As
such, `get_definition_with_timeout` essentially leaks an open
connection.

