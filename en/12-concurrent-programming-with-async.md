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

The Async OCaml library offers a hybrid model that lets you write
straight-line blocking code without using preemptive threading.
Let's dive into an example to see what this looks like, and
then explain some of the new concepts.  We're going to search for definitions
using the DuckDuckGo search engine, which exposes a HTTP/JSON API.

## Example: searching definitions with DuckDuckGo

A DuckDuckGo search is executed by making an HTTP request to `api.duckduckgo.com`. The result format comes back as either JSON or XML, depending on what was requested. Lets write some functions that construct the right URI and can parse the JSON:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
open Core.Std
open Async.Std

(* Generate a DuckDuckGo search URI from a query string *)
let ddg_uri =
  let uri = Uri.of_string ("http://api.duckduckgo.com/?format=json") in
  fun query ->
    Uri.add_query_param uri ("q", [query])

(* Extract the "Definition" field from the DuckDuckGo results *)
let get_definition_from_json json =
  match Yojson.Safe.from_string json with
  |`Assoc kv_list ->
      let open Option in
      List.Assoc.find kv_list "Definition" >>|
      Yojson.Safe.to_string
  |_ -> None
~~~~~~~~~~~~~~~~~~~~~~~~~~~

To compile this fragment, you will need to OPAM install `uri`
for the URI library and `yojson` for the JSON parsing.  The
`Uri` library takes care of encoding the URI for an HTTP request,
so you just specify your query as a normal OCaml string to the
`ddg_uri` function.

Yojson is a low-level JSON library which parses a string into
a matching OCaml tree. The JSON values are represented using polymorphic
variants, and so can be pattern matched on.  The `get_definition_from_json`
function does exactly this, and returns an optional *Definition* string
if one is found.  Note how we open the `Option` module here; this lets us map the search of the JSON list with a string conversion function. If no result is found, then the latter function is simply ignored, and a `None` returned. 

Now that we've written that boilerplate, let's look at the Async code to perform the actual search:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* Execute the DuckDuckGo search *)
(* TODO: This client API is being simplified in Cohttp *)
let ddg_query query =
  Cohttp_async.Client.call `GET (ddg_uri query)
  >>= function
  | Some (res, Some body) ->
      let buf = Buffer.create 128 in
      Pipe.iter_without_pushback body ~f:(Buffer.add_string buf)
      >>| fun () ->
      get_definition_from_json (Buffer.contents buf) |!
      Option.value ~default:"???"
  | Some (_, None) | None ->
      failwith "no body in response"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

For this portion of the code, you will need to OPAM install the `cohttp` library.

The core new Async concept is the `Deferred.t` type, which represents a *future* value who result is not available yet.  You can wait for the result by using the `>>=` operator (this is imported when you open `Async.Std`). Note that this is the same monad pattern available in other Core libraries such as `Option`, but instead of operating on optional values we are now mapping over future values. _(avsm: can I xref back to an explanation in the earlier sections about the Monad pattern?)_

The `ddg_query` function first invokes the HTTP client call, and subsequently converts the response body into a string and passes it through the JSON parser and finally returns a human-readable string.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* Run a single search *)
let run_one_search =
  ddg_query "Camel" >>| prerr_endline

(* Start the Async scheduler *)
let _ = Scheduler.go ()
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's actually use the search function now.
The fragment above first spawns a single search, and then fires up the Async scheduler.  The scheduler is where all the work happens, and must be started in every application that uses Async.  Without it, logging won't be output, nor will blocked functions ever wake up.

A single connection isn't that interested from a concurrency perspective, and Async makes it easy to run multiple parallel searches:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* Run many searches in parallel *)
let run_many_searches =
  let searches = ["Duck"; "Sheep"; "Cow"; "Llama"; "Camel"] in
  Deferred.List.map ~how:`Parallel searches ~f:ddg_query >>|
  List.iter ~f:print_endline
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `Deferred` library has a `List` module which lets you specify exactly how to map over a collection of future threads.  If you replace the `how` parameter with `Serial`, it will wait for each search to complete before issuing the next one.

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

(_avsm_: this utop feature not actually implemented yet for Async, but works for Lwt)

## Timing and Thread Composition

Our examples so far have been with static threads, and now we'll look at how to
coordinate multiple threads and timeouts.  Let's write a program that spawns
two threads, each of which sleep for some random time and return either
"Heads" or "Tails", and the quickest thread returns its value.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Thread_safe.block_on_async_exn flip ;;
# - : string * Time.Span.t * Time.Span.t = ("Heads!", 2.86113s, 3.64635s) 
# Thread_safe.block_on_async_exn flip ;;
# - : string * Time.Span.t * Time.Span.t = ("Tails!", 4.44979s, 2.14977s)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

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


