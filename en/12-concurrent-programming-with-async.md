# Concurrent Programming with Async

When you start building OCaml code that interfaces with external systems,
you'll soon need to handle concurrent operations. Consider the case of a web
server sending a large file to many clients, or a GUI waiting for a mouse
clicks. These applications often need to block while waiting for input for a particular task, and process something else during that time. Meanwhile, when new data does appear, the blocked task needs to be resumed as quickly as possible.

Busy servers can often handle tens of thousands of simultaneous connections, so runtime efficiency really matters.  An equally important concern is readable source code, so that the control flow of the program is obvious at a glance.

A common approach to concurrency is to use _preemptive_ system threads, most commonly in Java or C#.  In this threading model, each task is given an operating system thread of its own, and the kernel schedules them with arbitrary interleavings.
Other language runtimes such as Javascript are single-threaded, and applications register function callbacks to be triggered upon external events such as a timeout or browser click.

Both of these mechanisms have tradeoffs. Preemptive threads require more resources per thread and can be memory hungry. The operating system can also arbitrarily interleave the execution of preemptive threads, putting a load on the programmer to lock shared data structures.
Single-threaded event-driven systems execute a single task at a time and require less locking.  However, the program structure can often descend into a maze of event callbacks for even a simple operation that blocks a few times.  Code readability matters, and so we'd like to avoid such spaghetti control flow.

The `Async` OCaml library offers a hybrid model that lets you write
event-driven code that can block *without* the complexity of preemptive threading.
Lets begin by constructing a simple thread. Async follows the Core convention
and provides an `Async.Std` that provides threaded variants of many standard
library functions.

```ocaml
# require "async.unix" ;;
# open Async.Std ;;
# return 5 ;;
- : int Deferred.t = <abstr>
```
<note>
<title>When to open `Async.Std`</title>

The `Core.Std` module is normally opened up in every file you write. You
need to be a little more careful when opening `Async.Std` as it replaces
standard blocking functions with asynchronous equivalents.
This comes across most obviously with the standard input and output descriptors.

```ocaml
# open Core.Std;;
# print_endline "hello world";;
hello world
- : unit = ()
# open Async.Std;;
# print_endline "hello world";;
- : unit = ()
TODO what is the manual flush call here?
```

With just `Core.Std` open, the `print_endline` function immediately displayed
its output to the console.  When `Async.Std` was opened, the call to `print_endline`
is buffered and needs to be manually flushed before the output is displayed.

</note>

## Creating your first async threads

Async threads are co-operative and never preempt each other, and
the library internally converts blocking code into a single event loop.  The
threads are normal OCaml heap-allocated values (without any runtime magic!) and
can be allocated very fast. Concurrency is mostly limited only by your
available main memory, or operating system limits on other resources such
as file descriptors.

The basic type of an Async thread is a `'a Deferred.t`, which can be constructed
by the `return` function.  The type parameter (in this case `int`) represents
the ultimate type of the thread once it has completed in the future.  This
return value cannot be used directly while it is wrapped in a `Deferred.t` as
it may not be available yet.  Instead, we can `bind` a functional closure to
the thread that is called once the value is eventually ready.

```ocaml
# open Async.Std ;;
# let x = return 5 ;;
val x : int Deferred.t = <abstr>
```

The `return` function constructs a constant `int Deferred.t` whose value will
be available immediately.

```ocaml
# let y = Deferred.bind x (fun v -> return (string_of_int v)) ;;
val y : string Deferred.t = <abstr>
```

We've now bound a function to `x` that will be called over its resulting
value.  The closure simply converts the `int` to a `string` and returns it
as a `string Deferred.t`. 
Notice that while both `x` and `y` share a common `Deferred.t` type, their type
variables differ and so they cannot be interchangably used except in
polymorphic functions.  This is useful when refactoring large codebases as you
can tell if any function will block simply by the presence of an `Deferred.t`
in the signature.

Another important note is that the result of the bound function must also be
a `Deferred` value.  If we try to return a `string` immediately, then we get the
following type error.

````ocaml
# let y = Deferred.bind x (fun v -> string_of_int v);;
Error: This expression has type string but an expression was expected of type
         'a Deferred.t = 'a Ivar.Deferred.t
```

This requirement makes `bind` operations composable. You can take the
`y` value and `bind` it again to another thread, and expect them all to
run in the correct sequence.
Let's examine the function signatures of `bind` and `return` more closely
to understand this better.

```ocaml
# return ;;
- : 'a -> 'a Deferred.t = <fun>
# Deferred.bind ;;
- : 'a Deferred.t -> ('a -> 'b Deferred.t) -> 'b Deferred.t = <fun>
```

`return`, `bind` and the `Deferred.t` type all contain polymorphic type
variables (the `'a`) which represent the type of the thread, and are inferred
based on how they are used in your code. The `'a` type of the argument passed
to the `bind` callback _must_ be the same as the `'a Deferred.t` of the input
thread, preventing runtime mismatches between thread callbacks.

Both `bind` and `return` form a design pattern in functional programming known as *monads*, and
you will run across this signature in many applications beyond just threads.
_TODO avsm: figure out where to talk about all the monads in Core in more detail_.
The `>>=` inline operator is provided as a more succinct alias to `bind`, as shown below.

```ocaml
# let x = return 5 ;;
val x : int Deferred.t = <abstr>
# x >>= fun y -> return (string_of_int y) ;;
val - : string Deferred.t = <abstr>
```

The `>>=` operator is exactly the same as `bind` and unpacks the integer future
into the `y` variable. The subsequent closure is called with the resulting integer and
builds a new string future.

It can be a little verbose to keep calling `bind` and `return` to wrap simple
functions such as `string_of_int`. The `>>|` operator maps a non-Async function 
directly across a `Deferred.t` value.  In the example below, the deferred `x` is
mapped to `string_of_int` directly, and the result is a `string Deferred.t`.

```ocaml
# x >>| string_of_int ;;
val - : string Deferred.t = <abstr>
```

Multiple threads can be chained together with successive calls to `bind` to sequentially compose
blocking operations.

```ocaml
# return 5
  >>= fun v -> return (string_of_int v)
  >>= fun v -> return (v = "5")
- : bool Deferred.t = <abstr>
```

The example above constructs an `int` thread, converts it to a `string` thread, and then to
a `bool` thread via a string comparison.  Of course, there's no interesting threading
going on in this example beyond building a constant value, but let's look at how to run
it next.

## Executing async applications

All async threads run within a _scheduler_ that is responsible for associating
blocked threads with system resources (such as file descriptors) and waking them
up when external I/O or timer events fire. 

### Running threads within the toplevel

If you're experimenting with async programming, the `utop` toplevel is a convenient
place to write code interactively. Async threads can be evaluated into a concrete
value by wrapping them in `Thread_safe.block_on_async_exn`, which spawns a system thread that waits until
a result is available.

```ocaml
# Thread_safe.block_on_async_exn ;;
- : (unit -> 'a Deferred.t) -> 'a = <fun>
```
 
A neat feature in `utop` is that it detects functions with a
`Deferred.t` in the return type, and automatically translates it into
a call to `block_on_async_exn` for you.

```ocaml
# let fn () = return 5 >>| string_of_int ;;
val fn : unit -> string Deferred.t = <abstr>

# Thread_safe.block_on_async_exn fn ;;
- : string = "5"

# fn () ;;
- : string = "5"
```

We've defined an `fn` thread in the first phrase, and then run it manually
using `block_on_async_exn`.  The final phrase executes `fn` directly, and
you can see the `utop` translation kicking in and returning the concrete
value.

### Running threads within an application

An application can also use the same `block_on_async_exn` as the toplevel, but there are two alternatives.
Once a few threads have been started, the `Scheduler.go` function runs them for you.

```ocaml
# Scheduler.go ;;
- : ?raise_unhandled_exn:bool -> unit -> never_returns = <fun> 
```

Notice that this function never returns, even if all of the spawned threads are completed.
The Async scheduler doesn't terminate by default, and so most applications will listen for a signal to exit or simply use `CTRL-C` to interrupt it from a console.

Another common way to execute async threads is via the `Command` module we introduced in [xref](#command-line-parsing).
When you open `Async.Std`, the `Command` module now has an `async_basic` available.

```ocaml
# Command.async_basic ;;
- : summary:string -> 
    ?readme:(unit -> string) -> 
    ('a, unit -> unit Deferred.t) Command.Spec.t -> 
    'a -> Command.t = <fun>
```

This is used in exactly the same way as the usual `Command` module, except that the callbacks must return a `Deferred.t`.  This lets you run blocking threads directly from a command-line interface.

## Timing and Thread Composition

Our examples so far have been with static threads, which isn't
very much use for real programs.  We'll now add timing to the mix
by showing you how to  coordinate multiple threads and timeouts.
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

This example introduces a couple of new time-related Async functions. The `Time` module
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

```ocaml
# Thread_safe.block_on_async_exn flip ;;
# - : string * Time.Span.t * Time.Span.t = ("Heads!", 2.86113s, 3.64635s)
# Thread_safe.block_on_async_exn flip ;;
# - : string * Time.Span.t * Time.Span.t = ("Tails!", 4.44979s, 2.14977s)
```

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

<note><title>A Note on Portability</title>

Explain libev and why its needed here.

</note>

## Example: searching definitions with DuckDuckGo

DuckDuckGo is a search engine with a freely available search interface.
A DuckDuckGo search is executed by making an HTTP request to `api.duckduckgo.com`. The result comes back in either JSON or XML format, depending on what was requested in the original query string. Let's write some functions that construct the right URI and can parse the resulting JSON.

Before we can make the HTTP calls, we need a couple of helper functions with the following signature.

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

A `Uri.t` is constructed from the `Uri.of_string` function, and a query parameter `q` is added with the desired search query.  The library takes care of encoding the URI correctly when outputting it in the network protocol.

Note that the URI manipulation functions are all *pure* functions which return a new URI value, and never modify the input.  This makes it easier to pass around URI values through your application stack without fear of modification.

### Parsing JSON strings

The HTTP response from DuckDuckGo is in JSON, a common (and thankfully simple) format that is specified in [RFC4627](http://www.ietf.org/rfc/rfc4627.txt).  There are quite a few JSON parsers available for OCaml, and we've picked [`Yojson`](http://mjambon.com/yojson.html) for this example.

There are a few non-standard extensions to JSON, so Yojson exposes them as the `Basic` and `Safe` sub-modules.  It doesn't really matter which one we pick for this simple example, so we'll go with `Safe`.

The input `string` is parsed using `Yojson.Safe.from_string` into an OCaml data type. The JSON values are represented using polymorphic
variants, and can thus be pattern matched more easily once they have been parsed by Yojson.

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

We're expecting the DuckDuckGo response to be a record, with an optional `Description` field being one of the keys in the record.
The `get_definition_from_json` does a pattern match on this, and returns an optional string if a definition is found within the result.

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

Notice that we use options here instead of throwing exceptions on an error.
When the `Option` module is opened, it provides a `map` operator (`>>|`) which
calls the bound closure if the value exists.
If no result is found, then the `Yojson.Safe.to_string` conversion function is simply ignored, and a `None` returned.

### Executing an HTTP client query

Now that we've written those utility functions, let's look at the Async code that performs the actual search:

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

For this code, you'll need to OPAM install the `cohttp` library.  The `Cohttp_async.Client` module executes the HTTP call, and returns a status and response body wrapped.  This whole result is wrapped in a type you haven't seen before: `Async.Deferred.t`.

The `Deferred.t` represents a *future* value whose result is not available yet. You can "wait" for the result by binding a callback using the `>>=` operator (which is imported when you open `Async.Std`). This is the same monad pattern available in other Core libraries such as `Option`, but instead of operating on optional values, we are now mapping over future values.
We'll come back to monads later in this chapter. (_avsm_: TODO xref)

The `ddg_query` function invokes the HTTP client call, and returns a tuple containing the response codes and headers, and a `string Pipe.Reader`.  Pipes in Async are often used to transmit large amounts of data between two processes or concurrent threads.  The `Cohttp` library creates a `Pipe.Writer` which it outputs the HTTP body into, and provides your application with the `Reader` end.

In this case, the HTTP body probably isn't very large, so we just iterate over the Pipe's contents until we have the full HTTP body in a `Buffer.t`.
Once the full body has been retrieved into our buffer, the next callback passes it through the JSON parser and returns a human-readable string of the search description that DuckDuckGo gave us.

```ocaml
(* Run a single search *)
let run_one_search =
  do_ddg_query "Camel" >>| prerr_endline

(* Start the Async scheduler *)
let _ = Scheduler.go ()
```

Let's actually use the search function to run a real query now. The fragment above spawns a single search, and then fires up the Async scheduler.  The scheduler is where all the work happens, and must be started in every application that uses Async.  Without it, logging won't be output, nor will blocked functions ever wake up.
When the scheduler is active, it is waiting for incoming I/O events and waking up function callbacks that were sleeping on that particular file descriptor or timeout.

A single connection isn't that interesting from a concurrency perspective.
Luckily, Async makes it very easy to run multiple parallel searches:

```ocaml
(* Run many searches in parallel *)
let run_many_searches =
  let searches = ["Duck"; "Sheep"; "Cow"; "Llama"; "Camel"] in
  Deferred.List.map ~how:`Parallel searches ~f:do_ddg_query >>|
  List.iter ~f:print_endline
```

The `Deferred.List` module lets you specify exactly how to map over a collection of futures.  The searches will be executed simultaneously, and the map thread will complete once all of the sub-threads are complete. If you replace the `Parallel` parameter with `Serial`, the map will wait for each search to fully complete before issuing the next one.



