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








## Onto an HTTP Server

## Binding to the GitHub API

Show how we can use a monadic style to bind to the GitHub API and make
simple JSON requests/responses.

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

