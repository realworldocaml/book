# First class modules

You can think of OCaml as being broken up into two parts: a core
language that is concerned with values and types, and a module
language that is concerned with modules and module signatures.  These
sub-languages are stratified, in that modules can contain types and
values, but ordinary values can't contain modules or module types.
That means you can't do things like define a variable whose value is a
module, or a function that takes a module as an argument.

OCaml provides a way around this stratification in the form of
_first-class modules_.  First-class modules are ordinary values that
can be created from and converted back to regular modules.  As we'll
see, letting modules into the core language makes it possible build
more modular programs.

### A trivial example

As we did with functors, we'll start out by considering a trivial
example so we can cover the basic mechanics of first class modules
with a minimum of fuss.  We'll actually use essentially the same
example we used with functors: a module containing a single integer
variable.

A first-class module is created by packaging up a module with a
signature that it satisfies.  The following defines a simple signature
and a module that matches it.

```ocaml
# module type X_int = sig val x : int end;;
module type X_int = sig val x : int end
# module Three : X_int = struct let x = 3 end;;
module Three : X_int
# Three.x;;
- : int = 3
```

We can convert this ordinary module into a first-class module using
the `module` keyword.

```ocaml
# let three = (module Three : X_int);;
val three : (module X_int) = <module>
```

If the module type can be inferred, than you can even omit the
signature.  Thus, we can write:

```ocaml
# module Four = struct let x = 4 end;;
module Four : sig val x : int end
# let numbers = [ three; (module Four) ];;
val numbers : (module X_int) list = [<module>; <module>]
```

And we can even create a first-class module from an anonymous module:

```ocaml
# let numbers = [three; (module struct let x = 4 end)];;
val numbers : (module X_int) list = [<module>; <module>]
```

Note that the type of the first-class module, `(module X_int)`, is
based on the name of the signature that we used in constructing it.  A
first class module based on a signature with a different name, even if
it is substantively the same signature, will result in a distinct
type, as is shown below.

```ocaml
# module type Y_int = X_int;;
module type Y_int = X_int
# let five = (module struct let x = 5 end : Y_int);;
val five : (module Y_int) = <module>
# [three; five];;
Error: This expression has type (module Y_int)
       but an expression was expected of type (module X_int)
```

This constraint can occasionally be confusing.  The most common source
of confusion is if someone creates a module type aas a local alias to
a module type defined elsewhere, the two module types will lead to
distinct first class module types.

In order to access the contents of a first-class module, you need to
unpack it into an ordinary module.  This can be done using the `val`
keyword, as shown below.

```ocaml
# module New_three = (val three : X_int) ;;
module New_three : X_int
# New_three.x;;
- : int = 3
```

We can also write ordinary functions which consume and create first
class modules.  The following shows the definition of two function,
`to_int`, which converts a `(module X_int)` into an `int`.  And
`plus`, which adds two `(module X_int)`s.

```ocaml
# let to_int m =
    let module M = (val m : X_int) in
    M.x
  ;;
val to_int : (module X_int) -> int = <fun>
# let plus m1 m2 =
    (module struct
       let x = to_int m1 + to_int m2
     end : X_int)
  ;;
val plus : (module X_int) -> (module X_int) -> (module X_int) = <fun>
```

With these functions in hand, we can now work with `(module X_int)`'s
in a more natural style, taking full advantage of the concision and
simplicity of the core language.

```ocaml
# let six = plus three three;;
val six : (module X_int) = <module>
# to_int (List.fold ~init:six ~f:plus [three;three]);;
- : int = 12
```

Of course, all we've really done with this example is come up with a
more cumbersome way of working with integers.  In the following, we'll
look at more realistic examples.

## Dynamically choosing a module

Perhaps the simplest thing you can do with first-class modules that
you can't do without them is to pick the implementation of a module at
runtime.

Consider an application that does I/O multiplexing using a system call
like `select` to determine which file descriptors are ready to use.
There are in fact multiple APIs you might want to use, including
`select` itself, `epoll`, and `libev`, where different multiplexers
make somewhat different performance and portability trade-offs.  You
could support all of these in one application by defining a single
module, let's call it `Mutliplexer`, whose implementation is chosen at
runtime based on an environment variable.

To do this, you'd first need an interface `S` that all of the
different multiplexer implementations would need to match, and then an
implementation of each multiplexer.

```ocaml
(* file: multiplexer.ml *)

(* An interface the OS-specific functionality *)
module type S = sig ... end

(* The implementations of each individual multiplexer *)
module Select : S = struct ... end
module Epoll  : S = struct ... end
module Libev  : S = struct ... end
```

We can choose the first-class module that we want based on looking up
an environment variable.

```ocaml
let multiplexer =
  match Sys.getenv "MULTIPLEXER" with
  | None
  | Some "select" -> (module Select : S)
  | Some "epoll"  -> (module Epoll : S)
  | Some "libev"  -> (module Libev : S)
  | Some other -> failwithf "Unknown multiplexer: %s" other ()
```

Finally, we can convert the resulting first-class module back to an
ordinary module, and then include that so it becomes part of the body
of our module.

```ocaml
(* The final, dynamically chosen, implementation *)
include (val multiplexer : S)
```

## Example: A service bundle

This section describes the design of a library for bundling together
multiple services, where a service is a piece of code that exports a
query interface.  A service bundle combines together multiple
individual services under a single query interface that works by
dispatching incoming queries to the appropriate underlying service.

The following is a first attempt at an interface for our `Service`
module, which contains both a module type `S`, which is the interface
that a service should meet, as well as a `Bundle` module which is for
combining multiple services.

```ocaml
(* file: service.mli *)

open Core.Std

(** The module type for a service. *)
module type S = sig
  type t
  val name           : string
  val create         : unit -> t
  val handle_request : t -> Sexp.t -> Sexp.t Or_error.t
end

(** Bundles multiple services together *)
module Bundle : sig
  type t
  val create : (module S) list -> t
  val handle_request : t -> Sexp.t -> Sexp.t Or_error.t
  val service_names  : t -> string list
end
```

Here, a service has a state, represented by the type `t`, a name by
which the service can be referenced, a function `create` for
instantiating a service, and a function by which a service can
actually handle a request.  Here, requests and responses are delivered
as s-expressions.  At the `Bundle` level, the s-expression of a
request is expected to be formatted as follows:

```
(<service-name> <body>)
```

where `<service_name>` is the service that should handle the request,
and `<body>` is the body of the request.

Now let's look at how to implement `Service`.  The core datastructure
of `Bundle` is a hashtable of request handlers, one per service.
Each request handler is a function of type `(Sexp.t -> Sexp.t
Or_error.t)`.  These request handlers really stand in for the
underlying service, with the particular state of the service in
question being hidden inside of the request handler.

The first part of `service.ml` is just the preliminaries: the
definition of the module type `S`, and the definition of the type
`Bundle.t`.

```ocaml
(* file: service.ml *)

open Core.Std

module type S = sig
  type t
  val name           : string
  val create         : unit -> t
  val handle_request : t -> Sexp.t -> Sexp.t Or_error.t
end

module Bundle = struct
  type t = { handlers: (Sexp.t -> Sexp.t Or_error.t) String.Table.t; }
```

The next thing we need is a function for creating a `Bundle.t`.  This
`create` function builds a table to hold the request handlers, and
then iterates through the services, unpacking each module,
constructing the request handler, and then putting that request
handler in the table.

```ocaml
  (** Creates a handler given a list of services *)
  let create services =
    let handlers = String.Table.create () in
    List.iter services ~f:(fun service_m ->
      let module Service = (val service_m : S) in
      let service = Service.create () in
      if Hashtbl.mem handlers Service.name then
        failwith ("Attempt to register duplicate handler for "^Service.name);
      Hashtbl.replace handlers ~key:Service.name
        ~data:(fun sexp -> Service.handle_request service sexp)
    );
    {handlers}
```

Note that the `Service.t` that is created is referenced by the
corresponding request handler, so that it is effectively hidden behind
the function in the `handlers` table.

Now we can write the function for the bundle to handle requests.  The
handler will examine the s-expression to determine the body of the
query and the name of the service to dispatch to.  It then looks up
the handler calls it to generate the response.

```ocaml
  let handle_request t sexp =
    match sexp with
    | Sexp.List [Sexp.Atom name;query] ->
      begin match Hashtbl.find t.handlers name with
      | None -> Or_error.error_string ("Unknown service: "^name)
      | Some handler ->
        try handler query
        with exn -> Error (Error.of_exn exn)
      end
    | _ -> Or_error.error_string "Malformed query"
```

Last of all, we define a function for looking up the names of the
available services.

```ocaml
  let service_names t = Hashtbl.keys t.handlers

end
```

To see this system in action, we need to define some services, create
the corresponding bundle, and then hook that bundle up to some kind of
client.  For simplicity, we'll build a simple command-line interface.
There are two functions below: `handle_one`, which handles a single
interaction; and `handle_loop`, which creates the bundle and then runs
`handle_one` in a loop.

```ocaml
(* file: service_client.ml *)

open Core.Std

(** Handles a single request coming from stdin *)
let handle_one bundle =
  printf ">>> %!"; (* prompt *)
  match In_channel.input_line stdin with
  | None -> `Stop (* terminate on end-of-stream, so Ctrl-D will exit *)
  | Some line ->
    let line = String.strip line in (* drop leading and trailing whitespace *)
    if line = "" then `Continue
    else match Or_error.try_with (fun () -> Sexp.of_string line) with
    | Error err ->
      eprintf "Couldn't parse query: %s\n%!" (Error.to_string_hum err);
      `Continue
    | Ok query_sexp ->
      let resp = Service.Bundle.handle_request bundle query_sexp in
      Sexp.output_hum stdout (<:sexp_of<Sexp.t Or_error.t>> resp);
      Out_channel.newline stdout;
      `Continue

let handle_loop services =
  let bundle = Service.Bundle.create services in
  let rec loop () =
    match handle_one bundle with
    | `Stop -> ()
    | `Continue -> loop ()
  in
  loop ()
```

Now we'll create a couple of toy services.  One service is a counter
that can be updated by query; and the other service lists a directory.
The last line then kicks off the shell with the services we've
defined.

```ocaml
module Counter : Service.S = struct
  type t = int ref

  let name = "update-counter"
  let create () = ref 0

  let handle_request t sexp =
    match Or_error.try_with (fun () -> int_of_sexp sexp) with
    | Error _ as err -> err
    | Ok x ->
      t := !t + x;
      Ok (sexp_of_int !t)
end

module List_dir : Service.S = struct
  type t = unit

  let name = "ls"
  let create () = ()

  let handle_request () sexp =
    match Or_error.try_with (fun () -> string_of_sexp sexp) with
    | Error _ as err -> err
    | Ok dir -> Ok (Array.sexp_of_t String.sexp_of_t (Sys.readdir dir))
end

let () =
  handle_loop [(module List_dir : Service.S); (module Counter : Service.S)]
```

And now we can go ahead and start up the client.

```
$ ./service_client.byte
>>> (update-counter 1)
(Ok 1)
>>> (update-counter 10)
(Ok 11)
>>> (ls .)
(Ok
 (_build _tags service.ml service.mli service.mli~ service.ml~
  service_client.byte service_client.ml service_client.ml~))
>>>
```

Now, let's consider what happens to the design when we want to make
the interface of a service a bit more realistic.  In particular, right
now services are created without any configuration.  Let's add a
config type to each service, and change the interface of `Bundle` so
that services can be registered along with their configs.  At the same
time, we'll change the `Bundle` API to allow services to be changed
dynamically, rather than just added at creation time.










