# First class modules

<note>
<title>Note to reviewers</title>

This chapter is still incomplete, but the contents here are still
instructive enough that we decided to include it in the public
beta release.

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
see, letting modules into the core language increases the range of
what you can express and making it easier to build flexible and
modular systems.

## A trivial example

As we did with functors, we'll start out by considering a trivial
example so we can cover the basic mechanics with a minimum of fuss.
To taht end, consider the following signature of a module with a
single integer variable.

```ocaml
# module type X_int = sig val x : int end;;
module type X_int = sig val x : int end
```

And here is a module that matches that signature.

```ocaml
# module Three : X_int = struct let x = 3 end;;
module Three : X_int
# Three.x;;
- : int = 3
```

A first-class module is created by packaging up a module with a
signature that it satisfies.  We can convert the above module into a
first-class module as follows, using the `module` keyword.

```ocaml
# let three = (module Three : X_int);;
val three : (module X_int) = <module>
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

This constraint can occasionally be confusing.  In particular, if you
create a module type as an alias to a module type defined elsewhere,
the two module types will lead to distinct first class module types.

The module type doesn't always need to be part of the construction of
a first-class module.  In particular, we can omit the explicit
qualification by module type if that type can be inferred.  Thus, we
can write:

```ocaml
# module Four = struct let x = 4 end;;
module Four : sig val x : int end
# let numbers = [ three; (module Four) ];;
val numbers : (module X_int) list = [<module>; <module>]
```

We can also create a first-class module from an anonymous module:

```ocaml
# let numbers = [three; (module struct let x = 4 end)];;
val numbers : (module X_int) list = [<module>; <module>]
```

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
`plus`, which returns the sum of two `(module X_int)`s.

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

There are some useful syntactic shortcuts when dealing with first
class modules.  One notable one is that you can do the conversion to an
ordinary module within a pattern match.  Thus, we can rewrite the
`to_int` function as follows.

```ocaml
# let to_int (module M : X_int) = M.x ;;
val to_int : (module X_int) -> int = <fun>
```

## Example: A query handling framework

Now let's look at first class modules in the context of a more
complete and realistic module signature.  In particular, consider the
following signature for a module that implements a query handler.

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
signature, `with sexp` adds declarations of s-expression converters,
_e.g._,

```ocaml
# module type M = sig type t with sexp end;;
module type M =
  sig type t val t_of_sexp : Sexp.t -> t val sexp_of_t : t -> Sexp.t end
```

In a module, `with sexp` adds the implementation of those functions.
Thus, we can write

```ocaml
# type u = { a: int; b: float } with sexp;;
type u = { a : int; b : float; }
val u_of_sexp : Sexp.t -> u = <fun>
val sexp_of_u : u -> Sexp.t = <fun>
# sexp_of_u {a=3;b=7.};;
- : Sexp.t = ((a 3) (b 7))
# u_of_sexp (Sexp.of_string "((a 43) (b 3.4))");;
- : u = {a = 43; b = 3.4}
```

This is all described in more detail in
[xref](#data-serialization-with-s-expressions).

### Example query handlers

Let's look at some examples of query handlers that satisfy this
interface.  The following query hands out unique integer ids by
keeping an internal counter which it bumps every time it produces a
new value.  The input to the query in this case is just the trivial
s-expression `()` (otherwise known as `Sexp.unit`).

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
        let response = Ok (Int.sexp_of_t t.next_id) in
        t.next_id <- t.next_id + 1;
        response
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
# let unique = Unique.create 0;;
val unique : Unique.t = {Unique.next_id = 0}
# Unique.eval unique Sexp.unit;;
- : (Sexp.t, Error.t) Result.t = Ok 0
# Unique.eval unique Sexp.unit;;
- : (Sexp.t, Error.t) Result.t = Ok 1
```

Here's another example: a query handler that does directory listings.
Here, the config is the default directory that relative paths are
interpreted within.

```ocaml
# module List_dir = struct
    type config = string with sexp
    type t = { cwd: string }

    (** [is_abs p] Returns true if [p] is an absolute path  *)
    let is_abs p =
      String.length p > 0 && p.[0] = '/'

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
```

Again, we can create an instance of this query handler and interact
with it directly.

```ocaml
# let list_dir = List_dir.create "/var";;
val list_dir : List_dir.t = <abstr>
# List_dir.eval list_dir (sexp_of_string ".");;
- : (Sexp.t, Error.t) Result.t =
Ok
 (agentx at audit backups db empty folders jabberd lib log mail msgs named
  netboot pgsql_socket_alt root rpc run rwho spool tmp vm yp)
# List_dir.eval list_dir (sexp_of_string "yp");;
- : (Sexp.t, Error.t) Result.t =
Ok (binding binding~orig Makefile.main Makefile.yp)
```

### Dispatching to multiple query handlers

Now, what if we want to dispatch queries to any of an arbitrary
collection of handlers?  This is awkward to do with modules and
functors alone, but it's quite natural with first-class modules.  The
first thing we'll need to do is to create a signature that combines a
`Query_handler` module with an instantiated example of the handler.

```ocaml
# module type Query_handler_instance = sig
    module Query_handler : Query_handler
    val this : Query_handler.t
  end;;
module type Query_handler_instance =
  sig module Query_handler : Query_handler val this : Query_handler.t end
```

This will allow us to create a first-class module that encompases both
an instance of the query and the matching operations for working with
that query.  We can create first-class modules from this as follows.

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
       let this = List_dir.create "/var"
     end : Query_handler_instance);;
val list_dir_instance : (module Query_handler_instance) = <module>
```

Constructing these instances is a little verbose, so we can write a
function for doing it more concisely.

```ocaml
# let build_instance
        (type a)
        (module Q : Query_handler with type config = a)
        config
    =
    (module struct
       module Query_handler = Q
       let this = Q.create config
     end : Query_handler_instance)
  ;;
val build_instance :
  (module Query_handler with type config = 'a) ->
  'a -> (module Query_handler_instance) = <fun>
```

<note><title> Locally abstract types </title>

Here, we used a feature of OCaml that hasn't come up before: a locally
abstract type.  For any function, you can declare a pseudo-parameter
which is a fresh type that acts like an abstract type within the
context of the function, but which looks like a polymorphic type from
the outside.  Thus, the following compiles:

```ocaml
# (fun (type a) (x:a) -> x :: []);;
- : 'a -> 'a list = <fun>
```

because `a` is being used in a way that is compatible with it being
abstract.  But the compiler will complain if we write this:

```ocaml
# (fun (type a) (x:a) -> x + x);;
Error: This expression has type a but an expression was expected of type int
```

because we're assuming that `a` is an integer within the function.

One common use of locally abstract types is to create a new type that
can be used in constructing a module.  Here's an example of doing this
to create a new first-class module.

```ocaml
# module type Comparable = sig
    type t
    val compare : t -> t -> int
  end ;;
module type Comparable = sig type t val compare : t -> t -> int end
# let create_comparable (type a) compare =
    (module struct
       type t = a
       let compare = compare
     end : Comparable with type t = a)
  ;;
# create_comparable Int.compare;;
- : (module Comparable with type t = int) = <module>
# create_comparable Float.compare;;
- : (module Comparable with type t = float) = <module>
```

The same technique can be used to construct a local module that to be
fed to a functor.  More generally, locally abstract types are
important when you start wanting to work with modules in a more
dynamic way.

</note>

Using `build_instance`, constructing a new instance becomes a
one-liner:

```ocaml
# let unique_instance = build_instance (module Unique) 0;;
val unique_instance : (module Query_handler_instance) = <module>
# let list_dir_instance = build_instance (module List_dir)  "/var";;
val list_dir_instance : (module Query_handler_instance) = <module>
```

The following code lets you dispatch queries to one of a list of query
handlers.  Note that in the following we unpack the instance module
`I`, and then use the query handler instance itself (`I.this`) in
concert with the module that it was built out of (`I.Query_handler`).

```ocaml
# let dispatch_to_list handlers name_and_query =
    match name_and_query with
    | Sexp.List [Sexp.Atom name; query] ->
      let response =
        List.find_map handlers
          ~f:(fun (module I : Query_handler_instance) ->
            if I.Query_handler.name <> name then None
            else Some (I.Query_handler.eval I.this query)
          )
      in
      begin match response with
      | Some x -> x
      | None -> Or_error.error "Could not find matching handler"
                  name String.sexp_of_t
      end
    | _ ->
      Or_error.error_string "malformed query"
  ;;
val dispatch_to_list :
  (module Query_handler_instance) list -> Sexp.t -> Sexp.t Or_error.t = <fun>
```

The bundling together of the module and the value is in many ways
reminisicent of object-oriented languages.  One key difference, is
that first-class modules allow you to package up more than just a
functions or methods.  As we've seen, you can also include types and
even modules.  We've only used it in a small way here, but this extra
power allows you to build more sophisticated components that involve
multiple interdependent types and values.

Let's look at the final pieces we need to make this a complete running
example.  In order to access these query handlers, we'll build a
simple command-line interface for it, as follows.

```ocaml
# let rec cli handlers =
    printf ">>> %!";
    let result =
      match In_channel.input_line stdin with
      | None -> `Stop
      | Some line ->
        match Or_error.try_with (fun () -> Sexp.of_string line) with
        | Error e -> `Continue (Error.to_string_hum e)
        | Ok (Sexp.Atom "quit") -> `Stop
        | Ok query ->
          begin match dispatch_to_list handlers query with
          | Error e -> `Continue (Error.to_string_hum e)
          | Ok s    -> `Continue (Sexp.to_string_hum s)
          end;
    in
    match result with
    | `Stop -> ()
    | `Continue msg ->
      printf "%s\n%!" msg;
      cli handlers
  ;;
val cli : (module Query_handler_instance) list -> unit = <fun>
```

We can most effectively run this command-line interface from a
stand-alone program, which we can do by putting the above code in a
file along with following command to launch the interface.

```ocaml
let () =
  cli [unique_instance; list_dir_instance]
```

Here's an example of a session with this program.

```
$ ./query_handler.byte 
>>> (unique ())
0
>>> (unique ())
1
>>> (ls .)
(agentx at audit backups db empty folders jabberd lib log mail msgs named
 netboot pgsql_socket_alt root rpc run rwho spool tmp vm yp)
>>> (ls vm)
(sleepimage swapfile0 swapfile1 swapfile2 swapfile3 swapfile4 swapfile5
 swapfile6)
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

