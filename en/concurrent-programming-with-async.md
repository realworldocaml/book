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

## Building a message broker

Let's now walk through the task of building a simple, complete async
application.



## Building an Async application


An application can also use the same `block_on_async_exn` as the
toplevel, but there are two alternatives.  Once a few threads have
been started, the `Scheduler.go` function runs them for you.

```ocaml
# Scheduler.go ;;
- : ?raise_unhandled_exn:bool -> unit -> never_returns = <fun>
```

Notice that this function never returns, even if all of the spawned
threads are completed.  The Async scheduler doesn't terminate by
default, and so most applications will listen for a signal to exit or
simply use `CTRL-C` to interrupt it from a console.

Another common way to execute async threads is via the `Command`
module we introduced in [xref](#command-line-parsing).  When you open
`Async.Std`, the `Command` module now has an `async_basic` available.

```ocaml
# Command.async_basic ;;
- : summary:string ->
    ?readme:(unit -> string) ->
    ('a, unit -> unit Deferred.t) Command.Spec.t ->
    'a -> Command.t = <fun>
```

This is used in exactly the same way as the usual `Command` module,
except that the callbacks must return a `Deferred.t`.  This lets you
run blocking threads directly from a command-line interface.

## Timing and Thread Composition

Our examples so far have been with static threads, which isn't
very much use for real programs.  We'll now add timing to the mix
and show you how to coordinate threads and timeouts.
Let's write a program that spawns two threads, each of which sleep
for some random time and return either "Heads" or "Tails".
The first thread that wakes up returns its value.

```ocaml
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
```

This example introduces a couple of new time-related Async functions.
The `Time` module contains functions to express both absolute and
relative temporal relationships.  In our coin flipping example, we
use:
* `Time.Span.of_sec` to create a relative time span of 3 seconds
* `Time.Span.randomize` to permute this span randomly by 75%
* `Clock.after` to build a `unit Deferred.t` that will return after
  the specified timespan
* `Deferred.any` to select between a list of threads and return the
  value of the first one to return a value.

It's important to note that there is no need for an explicit "thread
create" function in Async.  Instead, we build up functions that
manipulate `Deferred.t` values, and bind them to names when
convenient.  In the example above, we've created `coin_heads` and
`coin_tails` which have the following type:

```
val coin_heads : (string * Time.Span.t * Time.Span.t) Deferred.t
val coin_tails : (string * Time.Span.t * Time.Span.t) Deferred.t
```

Both of the threads encode the time intervals in their return value so
that you can can easily verify the calculations (you could also simply
print the time spans to the console as they are calculated and
simplify the return types).  Let's verify this by running the `flip`
function at the toplevel a few times.  Remember to run this in `utop`,
since it will spin up the Async scheduler automatically for you and
block until a result is available.

```ocaml
# flip () ;;
# - : string * Time.Span.t * Time.Span.t = ("Heads!", 2.86113s, 3.64635s)

# flip () ;;
# - : string * Time.Span.t * Time.Span.t = ("Tails!", 4.44979s, 2.14977s)
```

We used `any` in our example to choose the first ready thread.  The
`Deferred` module has a number of other ways to select between
multiple threads:

Function    # Threads  Behaviour
--------    ---------  ---------
both        2          Combines both threads into a tuple and returns both values.
any         list       Returns the first thread that becomes determined.
all         list       Waits for all threads to complete and returns their values.
all_unit    list       Waits for all `unit` threads to complete and returns `unit`.
peek        1          Inspects a single thread to see if it is determined yet.

Try modifying the `Deferred.any` in the above example to use some of
the other thread joining functions above, such as `Deferred.both`.

### Cancellation

## A simple TCP Echo Server

## Onto an HTTP Server

## Binding to the GitHub API

Show how we can use a monadic style to bind to the GitHub API and make
simple JSON requests/responses.

<note><title>A Note on Portability</title>

Explain libev and why its needed here.

</note>

## Example: searching definitions with DuckDuckGo

DuckDuckGo is a search engine with a freely available search
interface.  A DuckDuckGo search is executed by making an HTTP request
to `api.duckduckgo.com`. The result comes back in either JSON or XML
format, depending on what was requested in the original query
string. Let's write some functions that construct the right URI and
can parse the resulting JSON.

Before we can make the HTTP calls, we need a couple of helper
functions with the following signature.

```ocaml
(* Generate a DuckDuckGo API search URI for [query] *)
val make_ddg_uri : query:string -> Uri.t

(* Extract the Definition field from the DuckDuckGo search
   response, or return [None] if it doesn't exist *)
val get_definition_from_json: string -> string option
```

This code uses a couple of new libraries we haven't seen before.  You will need
to OPAM install `uri` and `yojson` (refer to [xref](#installation) if you need
help).  Let's see how to implement them first.

### URI handling

You're hopefully familiar with HTTP URLs, which identify endpoints
across the World Wide Web.  These are actually part of a more general
family known as Uniform Resource Identifiers (URIs). The full URI
specification is defined in
[RFC3986](http://tools.ietf.org/html/rfc3986) (and is rather
complicated!).  Luckily, the `ocaml-uri` library provides a
strongly-typed interface which takes care of much of the hassle.

```ocaml
(* Generate a DuckDuckGo search URI from a query string *)
let make_ddg_uri =
  let base_uri = "http://api.duckduckgo.com/?format=json" in
  let uri = Uri.of_string base_uri in
  fun ~query ->
    Uri.add_query_param uri ("q", [query])
```

A `Uri.t` is constructed from the `Uri.of_string` function, and a
query parameter `q` is added with the desired search query.  The
library takes care of encoding the URI correctly when outputting it in
the network protocol.

Note that the URI manipulation functions are all *pure* functions
which return a new URI value, and never modify the input.  This makes
it easier to pass around URI values through your application stack
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
an optional string if a definition is found within the result.

```ocaml
(* Extract the Definition field from the DuckDuckGo search
   response, or return [None] if it doesn't exist *)
let get_definition_from_json (json:string) =
  match Yojson.Safe.from_string json with
  |`Assoc kv_list ->
      let open Option in
      List.Assoc.find kv_list "Definition" >>|
      Yojson.Safe.to_string
  |_ -> None
```

Notice that we use options here instead of throwing exceptions on an
error.  When the `Option` module is opened, it provides a `map`
operator (`>>|`) which calls the bound closure if the value exists.
If no result is found, then the `Yojson.Safe.to_string` conversion
function is simply ignored, and a `None` returned.

### Executing an HTTP client query

Now that we've written those utility functions, let's look at the
Async code that performs the actual search:

```ocaml
(* Execute the DuckDuckGo search *)
(* TODO: This client API is being simplified in Cohttp *)
let do_ddg_query query =
  Cohttp_async.Client.call `GET (make_ddg_uri ~query)
  >>= function
  | Some (res, Some body) ->
      let buf = Buffer.create 128 in
      Pipe.iter_without_pushback body ~f:(Buffer.add_string buf)
      >>| fun () ->
      get_definition_from_json (Buffer.contents buf)
      |> Option.value ~default:"???"
  | Some (_, None) | None ->
      failwith "no body in response"
```

For this code, you'll need to OPAM install the `cohttp` library.  The
`Cohttp_async.Client` module executes the HTTP call, and returns a
status and response body wrapped.  This whole result is wrapped in a
type you haven't seen before: `Async.Deferred.t`.

The `Deferred.t` represents a *future* value whose result is not
available yet. You can "wait" for the result by binding a callback
using the `>>=` operator (which is imported when you open
`Async.Std`). This is the same monad pattern available in other Core
libraries such as `Option`, but instead of operating on optional
values, we are now mapping over future values.  We'll come back to
monads later in this chapter. (_avsm_: TODO xref)

The `ddg_query` function invokes the HTTP client call, and returns a
tuple containing the response codes and headers, and a `string
Pipe.Reader`.  Pipes in Async are often used to transmit large amounts
of data between two processes or concurrent threads.  The `Cohttp`
library creates a `Pipe.Writer` which it outputs the HTTP body into,
and provides your application with the `Reader` end.

In this case, the HTTP body probably isn't very large, so we just
iterate over the Pipe's contents until we have the full HTTP body in a
`Buffer.t`.  Once the full body has been retrieved into our buffer,
the next callback passes it through the JSON parser and returns a
human-readable string of the search description that DuckDuckGo gave
us.

```ocaml
(* Run a single search *)
let run_one_search =
  do_ddg_query "Camel" >>| prerr_endline

(* Start the Async scheduler *)
let _ = Scheduler.go ()
```

Let's actually use the search function to run a real query now. The
fragment above spawns a single search, and then fires up the Async
scheduler.  The scheduler is where all the work happens, and must be
started in every application that uses Async.  Without it, logging
won't be output, nor will blocked functions ever wake up.  When the
scheduler is active, it is waiting for incoming I/O events and waking
up function callbacks that were sleeping on that particular file
descriptor or timeout.

A single connection isn't that interesting from a concurrency
perspective.  Luckily, Async makes it very easy to run multiple
parallel searches:

```ocaml
(* Run many searches in parallel *)
let run_many_searches =
  let searches = ["Duck"; "Sheep"; "Cow"; "Llama"; "Camel"] in
  Deferred.List.map ~how:`Parallel searches ~f:do_ddg_query >>|
  List.iter ~f:print_endline
```

The `Deferred.List` module lets you specify exactly how to map over a
collection of futures.  The searches will be executed simultaneously,
and the map thread will complete once all of the sub-threads are
complete. If you replace the `Parallel` parameter with `Serial`, the
map will wait for each search to fully complete before issuing the
next one.



## Other Stuff

<note>
<title>When to open `Async.Std`</title>

The `Core.Std` module is normally opened up in every file you
write. You need to be a little more careful when opening `Async.Std`
as it replaces standard blocking functions with asynchronous
equivalents.  This comes across most obviously with the standard input
and output descriptors.

```ocaml
# open Core.Std;;
# printf "%s %s!\n" "Hello" "World";;
hello world
- : unit = ()
# open Async.Std;;
# print_endline "hello world";;
- : unit = ()
```

With just `Core.Std` open, the `print_endline` function immediately
displayed its output to the console.  When `Async.Std` was opened, the
call to `print_endline` is buffered and needs to be manually flushed
before the output is displayed.

</note>

