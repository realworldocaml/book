# First class modules

<note><title> A note to reviewers</title>

This chapter is highly incomplete.  Proceed with caution...

</note>

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

## A trivial example

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

With these functions in hand, we can now work with values of type
`(module X_int)` in a more natural style, taking full advantage of the
concision and simplicity of the core language.

```ocaml
# let six = plus three three;;
val six : (module X_int) = <module>
# to_int (List.fold ~init:six ~f:plus [three;three]);;
- : int = 12
```

## Example: a query-handling service

In this section, we'll see what happens when we take a more complete
and realistic module signature as the basis for a first class module.
In particular, consider the following signature for a module that
implements a query handling service.

```ocaml
# module type Query_handler = sig

    (** Configuration for a query handler.  Note that this can be
        converted to and from an s-expression *)
    type config with sexp

    (** The name of the query-handling service *)
    val name : string

    (** The state o fthe query handler *)
    type t

    (** Create a new query handler from a config *)
    val create : config -> t

    (** Evaluate a given query, where both input and output are
        s-expressions *)
    val eval : t -> Sexp.t -> Sexp.t Or_error.t
  end;;
module type Query_handler =
  sig
    type config
    val name : string
    type t
    val create : config -> t
    val eval : t -> Sexp.t -> Sexp.t Or_error.t
    val config_of_sexp : Sexp.t -> config
    val sexp_of_config : config -> Sexp.t
  end
```

In the above we use s-expressions as the format for queries and
responses as well for the config.  S-expressions are a simple,
flexible, and human-readable serialization format commonly used in
Core.  We'll cover s-expressions in more detail in
[xref](#data-serialization-with-s-expressions), but for now, it's
enough to think of them as balanced parenthetical expressions whose
atomic values are strings, _e.g._, `(this (is an) (s expression))`.

In addition, we use the `sexplib` syntax extension which extends OCaml
by adding the `with sexp` declaration.  When attached to a type in a
signature adds in declarations of converters to and from
s-expressions.  In a module, `with sexp` adds the actual
implementation of those functions.  This is all described in more
detail in [xref](#data-serialization-with-s-expressions).

Let's give a couple of examples of simple services that satisfy this
interface.  For example, the following service hands out unique
integer ids by keeping an internal counter which it bumps every time
it produces a counter.  The input to the query in this case is just
the trivial s-expression `()`.

```ocaml
# module Unique = struct
    type config = int with sexp
    type t = { mutable next_id: int }

    let name = "unique"
    let create start_at = { next_id = start_at }

    let eval t sexp =
      match Or_error.try_with (fun () -> unit_of_sexp sexp) with
      | Error _ as err -> err
      | Ok () ->
        let resp = Ok (Int.sexp_of_t t.next_id) in
        t.next_id <- t.next_id + 1;
        resp
  end;;
module Unique :
  sig
    type config = int
    val config_of_sexp : Sexp.t -> config
    val sexp_of_config : config -> Sexp.t
    type t = { mutable next_id : config; }
    val name : string
    val create : config -> t
    val eval : t -> Sexp.t -> (Sexp.t, Error.t) Result.t
  end
```

We can use this module to create an instance of the `Unique` query
handler and interact with it.

```ocaml
# let config = Unique.config_of_sexp (Sexp.of_string "0");;
val config : Unique.config = <abstr>
# let unique = Unique.create config;;
val unique : Unique.t = <abstr>
# Unique.eval unique (Sexp.of_string "()");;
- : Sexp.t Or_error.t = Core_kernel.Result.Ok 0
# Unique.eval unique (Sexp.of_string "()");;
- : Sexp.t Or_error.t = Core_kernel.Result.Ok 1
```

Now, consider what happens if we have multiple such modules.  Here,
for example, is a query handler for handling directory listings.

```ocaml
# module List_dir = struct
    type config = string with sexp
    type t = { cwd: string }

    let is_abs s =
      String.length s > 0 && s.[0] = '/'

    let name = "ls"
    let create cwd = { cwd }

    let eval t sexp =
      match Or_error.try_with (fun () -> string_of_sexp sexp) with
      | Error _ as err -> err
      | Ok dir ->
        let dir =
          if is_abs dir then dir 
          else Filename.concat t.cwd dir
        in
        Ok (Array.sexp_of_t String.sexp_of_t (Sys.readdir dir))
  end;;
module List_dir :
  sig
    type config = string
    val name : config
    type t
    val create : config -> t
    val eval : t -> Sexp.t -> Sexp.t Or_error.t
    val config_of_sexp : Sexp.t -> config
    val sexp_of_config : config -> Sexp.t
  end
# let list_dir = List_dir.create "/";;
val list_dir : List_dir.t = <abstr>
# List_dir.eval list_dir (sexp_of_string "var");;
- : Sexp.t Or_error.t =
Core_kernel.Result.Ok
 (agentx at audit backups db empty folders jabberd lib log mail msgs named
  netboot pgsql_socket_alt root rpc run rwho spool tmp vm yp)
```

Now, what if we want to dispatch queries to any of an arbitrary
collection of handlers?  This is difficult to do with modules and
functors alone, but it's quite natural with first-class modules.  The
first thing we'll need to do is to create a signature that combines a
`Query_handler` module with an instantiated example of the handler, as
follows.

```ocaml
# module type Query_handler_instance = sig
    module Query_handler : Query_handler
    val this : Query_handler.t
  end;;
module type Query_handler_instance =
  sig module Query_handler : Query_handler val this : Query_handler.t end
```

We can create first-class modules of this type easily enough, if we
have both the `Query_handler` module and a config to create it from.

```ocaml
# let unique_instance =
    (module struct
       module Query_handler = Unique
       let this = Unique.create 0
     end : Query_handler_instance);;
val unique_instance : (module Query_handler_instance) = <module>
# let list_dir_instance =
    (module struct
       module Query_handler = List_dir
       let this = List_dir.create "/"
     end : Query_handler_instance);;
val list_dir_instance : (module Query_handler_instance) = <module>
```

Here's some code for putting together a simple interactive prompt that
lets you dispatch queries to a given query handler by name.

```ocaml
# let dispatch_to_list handlers name_and_query =
    let (name,query) = <:of_sexp<string * Sexp.t>> name_and_query in
    let response =
      List.find_map handlers
        ~f:(fun (module I : Query_handler_instance) ->
          if I.Query_handler.name <> name then None
          else Some (I.Query_handler.eval I.this query)
        )
    in
    match response with
    | Some x -> x
    | None -> Or_error.error "Could not find matching handler"
                name String.sexp_of_t
  ;;
val dispatch_to_list :
  (module Query_handler_instance) list -> Sexp.t -> Sexp.t Or_error.t = <fun>
```

And using this, we can create a simple interactive tool for sending
queries and viewing the results.

```ocaml
# let run s =
    match dispatch_to_list [ unique_instance; list_dir_instance ]
            (Sexp.of_string s)
    with
    | Error e -> printf "ERROR: %s\n" (Error.to_string_hum e)
    | Ok s -> print_endline (Sexp.to_string_hum s)
  ;;
val run : string -> unit = <fun>
```

```ocaml
# let rec repl handlers =
    printf ">>> %!";
    match In_channel.input_line stdin with
    | None -> ()
    | Some line ->
      match Or_error.try_with (fun () -> Sexp.of_string line) with
      | Error e -> printf "PARSE ERROR: %s\n%!" (Error.to_string_hum e)
      | Ok (Sexp.Atom "quit") -> ()
      | Ok query ->
        begin match dispatch_to_list handlers query with
        | Error e -> printf "ERROR: %s\n%!" (Error.to_string_hum e)
        | Ok s -> printf "%s\n%!" (Sexp.to_string_hum s)
        end;
        repl handlers
  ;;
```




```ocaml
# let handle_query handlers =
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
      | Ok  ->
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




## BREAK



Here, requests and responses are delivered as s-expressions, which are
a simple, flexible, and human-readable serialization format commonly
used in Core.  We'll cover s-expressions in more detail in
[xref](#data-serialization-with-s-expressions), but for now, it's
enough to think of them as balanced parenthetical expressions whose
atomic values are strings.



```ocaml
# module type Number = sig
    type t
    val zero : t
    val one : t
    val ( + ) : t -> t -> t
    val ( /% ) : t -> t -> t  
    val ( / ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( > ) : t -> t -> bool
    val ( = ) : t -> t -> bool
    val ( < ) : t -> t -> bool
  end;;
```

```ocaml
# let gcd (module N : Number) n m =
     let rec gcd n m = 
        if N.(n > m) then gcd m n
        else if N.(n = zero) then m
        else gcd n N.(m - n)
     in
     gcd n m
  ;;
```


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
multiple services, where a service is a component that exports a query
interface.  A service bundle combines together multiple individual
services under a single query interface that works by dispatching
incoming queries to the appropriate underlying service.  We'll use
this to build a simple command-line interface for interacting with
these services, but the basic idea could be used in other contexts,
like building a system for managing web services.

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
as s-expressions, which are a very simple serialization format
commonly used in Core.  We'll cover s-expressions in more detail in
[xref](#data-serialization-with-s-expressions), but for now, it's
enough to think of them as balanced parenthetical expressions whose
atomic values are strings.

At the `Bundle` level, the s-expression of a request is expected to be
formatted as follows:

```
(<service-name> <body>)
```

where `<service_name>` is the service that should handle the request,
and `<body>` is the body of the request.

Now let's look at how to implement `service.ml`.  We'll start with the
definition of the module type `S` and the definition of the type
`Bundle.t`.  A `Bundle.t` is implemented as a hashtable of request
handlers, one per service.  Each request handler is a function of type
`(Sexp.t -> Sexp.t Or_error.t)`.  These request handlers really stand
in for the underlying service, with the particular state of the
service in question being hidden behind the function.

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

Now we can write the function to handle requests.  The handler will
examine the s-expression to determine the body of the query and the
name of the service to dispatch to.  It then looks up the handler
calls it to generate the response.

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



