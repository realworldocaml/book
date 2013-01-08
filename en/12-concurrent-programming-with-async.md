# Concurrent Programming with Async

When you start building OCaml code that interfaces with external systems,
you'll soon need to handle concurrent operations. Consider the case of a web
server sending a large file to many clients, or a GUI waiting for a mouse
clicks. These applications often need to block while waiting for input for a particular task, and process something else during that time. Meanwhile, when new data does appear, the blocked task needs to be resumed as quickly as possible.

Efficiency really matters here, as busy servers can often handle tens of thousands of simultaneous connections.  An equally important concern is readable source code, where the control flow of the program is obvious at a glance.

You've probably used preemptive system threads before in some programming languages such as Java or C#.  In this model, each task is usually given an operating system thread of its own.
Other languages such as Javascript are single-threaded, and applications must register function callbacks to be triggered upon external events (such as a timeout or browser click).

Both of these mechanisms have tradeoffs. Preemptive threads require their own memory stacks and can be memory hungry. The operating system can also arbitrarily interleave the execution of threads, and so they require careful locking around shared data structures.

Event-driven systems usually only execute a single task at a time and require less locking.  However, the program structure can often descend into a maze of event callbacks for even a simple operation that blocks a few times.  Code readability matters, and so we'd like to avoid such spaghetti control flow.

The `Async` OCaml library offers a hybrid model that lets you write
event-driven code that can block *without* the complexity of preemptive threading.
Let's dive straight into an example to see what this looks like, and
then explain some of the new concepts.  We're going to search for definitions of English terms using the DuckDuckGo search engine.

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

This code uses a couple of new libraries we haven't seen before.
You will need to OPAM install `uri` and `yojson` (refer to chapter {{{installation}}} if you need help).  Let's see how to implement them first.

### URI handling

You're hopefully familiar with HTTP URLs, which identify endpoints across the World Wide Web.  These are actually part of a more general family known
as Uniform Resource Identifiers (URIs). The full URI specification is defined in [RFC3986](http://tools.ietf.org/html/rfc3986) (and is rather complicated!).
Luckily, the `ocaml-uri` library provides a strongly-typed interface which takes care of much of the hassle.

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
      get_definition_from_json (Buffer.contents buf) |!
      Option.value ~default:"???"
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

<note>
<title>Terminating Async applications</title>

When you run the search example, you'll notice that the application doesn't terminate even when all of the searches are complete. The Async scheduler doesn't terminate by default, and so most applications will listen for a signal to exit or simply use `CTRL-C` to interrupt it from a console.

Another alternative is to run an Async function in a separate system
thread. You can do this by wrapping the function in the `Async.Thread_safe.block_on_async_exn`.  The `utop` top-level does this automatically for you if you attempt to evaluate an Async function interactively.

</note>

## Manipulating Async threads

Now that we've seen the search example above, let's examine how Async works in more detail.

Async threads are co-operative and never preempt each other, and
the library internally converts blocking code into a single event loop.  The
threads are normal OCaml heap-allocated values (without any runtime magic!) and
are therefore very fast to allocate. Concurrency is mostly limited only by your
available main memory, or operating system limits on non-memory resources such
as file descriptors.

Lets begin by constructing a simple thread. Async follows the Core convention
and provides an `Async.Std` that provides threaded variants of many standard
library functions.  The examples throughout this chapter assume that `Async.Std`
is open in your environment.

```ocaml
# require "async.unix" ;;
# open Async.Std ;;
# return 5 ;;
- : int Deferred.t = <abstr>
```

The basic type of an Async thread is a `Deferred.t`, which can be constructed
by the `return` function.  The type parameter (in this case `int`) represents
the ultimate type of the thread once it has completed in the future.  This
return value cannot be used directly while it is wrapped in a `Deferred.t` as
it may not be available yet.  Instead, we `bind` a function closure that is
called once the value is eventually ready.

```ocaml
# let x = return 5 ;;
val x : int Deferred.t = <abstr>
# let y = Deferred.bind x (fun a -> return (string_of_int a)) ;;
val y : string Deferred.t = <abstr>
```

Here, we've bound a function to `x` that will convert the `int` to a `string`.
Notice that while both `x` and `y` share a common `Deferred.t` type, their type
variables differ and so they cannot be interchangably used except in
polymorphic functions.  This is useful when refactoring large codebases, as you
can tell if any function will block simply by the presence of an `Deferred.t`
in the signature.

Let's examine the function signatures of `bind` and `return` more closely.

```ocaml
# return ;;
- : 'a -> 'a Deferred.t = <fun>
# Deferred.bind ;;
- : 'a Deferred.t -> ('a -> 'b Deferred.t) -> 'b Deferred.t = <fun>
```

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

```ocaml
# let x = return 5 ;;
val x : int Deferred.t = <abstr>
# x >>= fun y -> return (string_of_int y) ;;
val - : string Deferred.t = <abstr>
# x >>| string_of_int ;;
val - : string Deferred.t = <abstr>
```

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

```ocaml
# let fn () = return 5 >>| string_of_int ;;
val fn : unit -> string Deferred.t = <abstr>
# Thread_safe.block_on_async_exn fn ;;
- : string = "5"
# fn () ;;
- : string = "5"
```

In the second evaluation of `fn`, the top-level detected the return type of
a future and evaluated the result into a concrete string.

(_avsm_: this utop feature not actually implemented yet for Async, but works for Lwt)

## Timing and Thread Composition

Our examples so far have been with static threads, and now we'll look at how to
coordinate multiple threads and timeouts.  Let's write a program that spawns
two threads, each of which sleep for some random time and return either
"Heads" or "Tails", and the quickest thread returns its value.

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

<sidebar><title>A Note on Portability</title>

Explain libev and why its needed here.

</sidebar>


