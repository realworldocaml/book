# First class modules

_(jyh: I'm going to start some new text on FCM.  We might want another
chapter, but let's see how it goes.  I've kept Ron's original text
below.)

OCaml provides several mechanisms for organizing your programs,
including modules and functors, files and compilation units, and
classes and objects.  Files and compilation units (`.ml` and `.mli`
files) are really just a simplified module system.  Classes and
objects are a different form of organization altogether (as we'll see
in [xref](#object-oriented-programming).  Yet, in each of these cases,
there is a clear separation between types and values -- values cannot
contain types, and types cannot contain values.  And since modules can
contain types, modules can't be values.

_(yminsky: Instead of saying that ml and mli files are a simplified
module system, maybe say that they "provide a simple way of creating
modules and interfaces", or some such?  It's not like there's a
simplified module system floating around)_

_(yminsky: consider dropping "Yet" in the above.)_

Next, we'll relax this restriction with _first-class modules_.
"First-class" means that modules can be passed around as ordinary
values that can be created from and converted back to regular modules.
This is a relatively recent addition to the OCaml language, and while
it might seem trivial to say, it has profound consequences on the
language.  First-class modules are strictly more expressive than any
other organization mechanism, including classes and objects.  Once you
use first-class modules, you'll never want to go back.

_(yminsky: I wouldn't say they're strictly more expressive.  For
example, they don't give you a way of expressing sub typing
relationships effectively, which objects do.)_

This is not say that first-class modules should be used
indiscriminately.  When you pass modules as values, the reason is to
support dynamic behavior, and this can have a negative impact on
understandability.  As we proceed, we'll compare first-class modules
to other techniques, and suggest alternatives when it seems
appropriate.

_(jyh: Original text
You can think of OCaml as being broken up into two sub-language: a
core language that is concerned with values and types, and a module
language that is concerned with modules and module signatures.  These
sub-languages are stratified, in that modules can contain types and
values, but ordinary values can't contain modules or module types.
That means you can't do things like define a variable whose definition
is a module, or a function that takes a module as an argument.

OCaml provides a way around this stratification in the form of
_first-class modules_.  First-class modules are ordinary values that
can be created from and converted back to regular modules.  As we'll
see, letting modules into the core language makes it possible to use
more flexible and dynamic module-oriented designs.)_

### Another trivial example

Much as we did with functors, we'll start out with an utterly trivial
example, to allow us to show the basic mechanics of first class
modules with a minimum of fuss.

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

We can then create a first-class module using the `module` keyword.

```ocaml
# let three = (module Three : X_int);;
val three : (module X_int) = <module>
```

Note that the type of the first-class module, `(module X_int)`, is
based on the name of the signature that we used in constructing it.

To get at the contents of `three`, we need to unpack it into a module
again, which we can do using the `val` keyword.

```ocaml
# module New_three = (val three : X_int) ;;
module New_three : X_int
# New_three.x;;
- : int = 3
```

Using these conversions as building blocks, we can create tools for
working with first-class modules in a natural way.  The following
shows the definition of two function, `to_int`, which converts a
`(module X_int)` into an `int`.  And `plus`, which adds two `(module
X_int)`s.

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

With these functions in hand, we can start operating on our `(module
X_int)`'s in a more natural style, taking full advantage of the
concision and simplicity of the core language.

```ocaml
# let six = plus three three;;
val six : (module X_int) = <module>
# to_int (List.fold ~init:six ~f:plus [three;three]);;
- : int = 12
```

Of course, all we've really done with this example is come up with a
more cumbersome way of working with integers.  Let's see what happens
when with work with more complex abstract types.

### Standard vs. first-class modules

_(yminsky: I'm not in solve with the example.  It feels in some sense
too artificial, and that aside, when you get to the end of the
example, you haven't really gotten any juice of first-class modules)_

_(yminsky: using "standard" in quotes seems a little awkward.  Maybe
just drop the quotes, and talk about standard or ordinary modules
directly?)_

Let's compare the style of "standard" modules to first-class modules,
using a simple library of abstract geometric shapes.  In a "standard"
module definition, we would define the shapes using abstract data
types, where there is a type `t` that defines the actual
representation, and the module would include functions that operate on
the values of type `t`.  In the following code, the module type
`Shape` defines the type of generic shape, and the modules
`Rectangle` and `Line` implement some concrete shapes.

```ocaml
module type Shape = sig
  type t
  val area : t -> int
  val position : t -> int * int
end

module Rectangle = struct
   type t = { width : int; height : int; x : int; y : int }
   let make ~x ~y ~width ~height =
      { width = width; height = height; x = x; y = y }
   let area { width = width; height = height } = width * height
   let position { x = x; y = y } = (x, y)
end

module Line = struct
   type t = { dx : int; dy : int; x : int; y : int }
   let make ~x ~y ~dx ~dy = { dx = dx; dy = dy; x = x; y = y }
   let area _ = 0
   let position { x = x; y = y } = (x, y)
end
```

Next, if we want to define a generic shape that is either a rectangle
or a line, we would probably use a variant type.  The following module
`Shapes` is entirely boilerplate.  We define the variant type, then
functions to perform a dynamic dispatch based on the type of object.

```ocaml
module Shapes = struct
   type t = [ `Rect of Rectangle.t | `Line of Line.t ]
   let make_rectangle = Rectangle.make
   let make_line = Line.make
   let area = function
      `Rect r -> Rectangle.area r
    | `Line l -> Line.area l
   let position = function
      `Rect r -> Rectangle.position r
    | `Line l -> Line.position l
end;;
```

In fact, confronted with this boilerplate, we would probably choose
not use modules at all, but simply define a single module with a
variant type and the code for all of the shapes.  This isn't to say
that separate code for separate shapes is bad, it just means that the
language doesn't support it well (at least with standard modules).

With first-class modules, the situation changes, but we have to
dispense with the representation type altogether.  For immutable
shapes, the implementation is now trivial.

```ocaml
# module type Shape = sig
    val area : int
    val position : int * int
  end;;
module type Shape = sig val area : int val position : int * int end
# let make_rectangle ~x ~y ~width ~height =
   let module Rectangle = struct
      let area = width * height
      let position = (x, y)
   end in
   (module Rectangle : Shape);;
val make_rectangle :
  x:int -> y:int -> width:int -> height:int -> (module Shape) = <fun>
# let make_line ~x ~y ~dx ~dy =
   let module Line = struct
      let area = 0
      let position = (x, y)
   end in
   (module Line : Shape);;
val make_line : x:int -> y:int -> dx:'a -> dy:'b -> (module Shape) = <fun>
```

For mutable shapes, it isn't much different, but we have to include
the state as values in the module implementations.  For this, we'll
define a representation type `t` in the module implementation, and for
rectangles, a value `rect` of that type.  The code for lines is
similar.

```ocaml
# module type Shape = sig
     val area : unit -> int
     val position : unit -> int * int
     val moveby : dx:int -> dy:int -> unit
     val enlargeby : size:int -> unit
  end;;
module type Shape = ...
# let make_rectangle ~x ~y ~width ~height =
    let module Rectangle = struct
      type t = { mutable x : int; mutable y : int;
                 mutable width : int; mutable height : int }
      let rect = { x = x; y = y; width = width; height = height }
      let area () = rect.width * rect.height
      let position () = (rect.x, rect.y)
      let moveby ~dx ~dy =
         rect.x <- rect.x + dx;
         rect.y <- rect.y + dy
      let enlargeby ~size =
         rect.width <- rect.width * size;
         rect.height <- rect.height * size
    end in
    (module Rectangle : Shape);;
val make_rectangle :
  x:int -> y:int -> width:int -> height:int -> (module Shape) = <fun>
```

### A more complete example -- containers

So far, we haven't done anything that really needs modules.  The type
`Shape` could just as well be specified as a record type `type shape =
{ area : int; position : int * int; ... }`.

To explore the topic more fully, let's implement a system of dynamic
containers.  OCaml already provides a set of standard containers like
`List`, `Set`, `Hashtbl`, etc., but these types have to be selected
statically.  If a function expects a value of type
`Set.Make(ElementType).t`, then you have to pass it a set of exactly
that type.  What we would like is a kind of container where the
container implementation is chosen by the caller.  We define an
abstract _interface_, as a module type, then define one or more
concrete module implementations.

Let's start by defining an abstract container interface.  It contains
some elements of type `elt`, and functions to examine and iterate
through the contents.  For convenience, we also define a normal type
`'a container` to represent containers with elements of type `'a`.

```ocaml
module type Container = sig
   type elt
   val empty : unit -> bool
   val iter : (elt -> unit) -> unit
   val fold : ('a -> elt -> 'a) -> 'a -> 'a
end;;

type 'a container = (module Container with type elt = 'a)
```

#### Imperative containers

For imperative containers, will also want functions to mutate the
contents by adding or removing elements.  For example, a stack can be
implemented as a module `Stack` that includes all the functions in the
generic `Container` module, as well as functions to push and pop
elements.

```ocaml
module type Stack = sig
   include Container
   val push : elt -> unit
   val pop : unit -> elt
end;;

type 'a stack = (module Stack with type elt = 'a)
```

Now that the types are defined, the next step is to define a concrete
container implementation.  For this simple example, we'll use a list
to represent a stack.  The function `make_list_stack` constructs module
implementation using a `let module` construction, then returns the result.

```ocaml
# let make_list_stack (type element) () : element stack =
    let module ListStack = struct
      type elt = element
      let contents = ref []
      let empty () = !contents = []
      let iter f = List.iter f !contents
      let fold f x = List.fold_left f x !contents
      let push x = contents := x :: !contents
      let pop () =
         match !contents with
            x :: rest -> contents := rest; x
          | [] -> raise (Invalid_argument "stack is empty")
    end in
    (module ListStack : Stack with type elt = element);;
val make_list_stack : unit -> 'a stack = <fun>
```


Note the use of the explicit type parameter `element`.  This is
required because the use of a type variable in the module definition
(like `type elt = 'a`) would be rejected by the compiler.  The
construction and use of the stack is straightforward.

```ocaml
# let demo (s : int stack) =
    let module S = (val s) in
    S.push 5;
    S.push 17;
    S.iter (fun i -> Printf.printf "Element: %d\n" i);;
val demo : int stack -> unit = <fun>
# demo (make_list_stack ());;
Element: 17
Element: 5
- : unit = ()
```

The `demo` function is entirely oblivious to the implementation of the
stack.  Instead of passing a module implementation based on lists, we
could pass a different implementation based on arrays.

We could go on to define other containers, sets, dictionaries, queues,
etc. but the implementations would be similar to what we have seen.
Instead, let's look at functional data structures, which require a
little more work to express.

#### Pure functional containers

Imperative data structures have simpler types that functional ones
because the return type of imperative functions is just `unit`.  When
we look at pure functional data structures, we immediately run into a
problem with type recursion.

```ocaml
# module type Container = sig
    type elt
    val empty : bool
    val iter : (elt -> unit) -> unit
    val fold : ('a -> elt -> 'a) -> 'a -> 'a
    val add : elt -> (module Container)
  end;;
Characters 160-178:
     val add : elt -> (module Container)
                      ^^^^^^^^^^^^^^^^^^
Error: Unbound module type Container
```

The problem here is that module type definitions are not recursive --
we can't use the type being defined in its own definition.

Recursive modules provide a solution, but it requires a "trick", where
we define a module that is equal to itself.  This module contains only
type definitions, and the only purpose of the outer recursive module
is to allow the recursion in the definition.  While we're at it, let's
include a `map` function with the usual semantics.

```ocaml
module rec Container : sig
   module type T = sig
      type elt
      val empty : bool
      val iter : (elt -> unit) -> unit
      val fold : ('a -> elt -> 'a) -> 'a -> 'a
      val map : (elt -> 'a) -> 'a Container.t
      val add : elt -> elt Container.t
   end
   type 'a t = (module Container.T with type elt = 'a)
end = Container;;
```

There are several ways to write this model, but this definition is
convenient because it defines both a module type `Container.T` and a
value type `'a Container.t`.  The outer recursive module `Container`
allows the module type `T` to refer to the value type `t` and _vice
versa_.  Note that the module `Container` is defined as itself (as
`Container`).

With this first technicality out of the way, the next one is how to
construct values of type `Container.t`.  In the imperative version of
the stack, we used a function `make_list_stack`.  We want to do the
same here, but the function definition must be both recursive and
polymorphic.

```ocaml
# let make_stack () =
    let rec make : 'a. 'a list -> 'a Container.t = fun
      (type element) (contents : element list) ->
      let module NewList = struct
         type elt = element
         let empty = contents = []
         let iter f = List.iter f contents
         let fold f x = List.fold_left f x contents
         let map f = make (List.map f contents)
         let add x = make (x :: contents)
      end in
      (module NewList : Container.T with type elt = element)
   in
   make [];;
val make_stack : unit -> 'a Container.t = <fun>
```

The recursion here is particularly important.  The functions `map` and
`add` return new collections, so they call the function `make`
recursively.  The explicit polymorphic type `make : 'a. 'a list -> 'a
Container.t` means that the function `make` is properly polymorphic,
so that the `map` function is polymorphic.

Now that the construction is done, the usage is similar to the
imperative case, except that now the data structure is functional.

```ocaml
# let demo (s : int Container.t) =
    let module S = (val s) in
    let module S = (val (S.add 5)) in
    let module S = (val (S.add 17)) in
    S.iter (fun i -> Printf.printf "Int Element: %d\n" i);
    let s = S.map (fun i -> float_of_int i +. 0.1) in
    let module S = (val s) in
    S.iter (fun x -> Printf.printf "Float Element: %f\n" x);
    s;;
val demo : int Container.t -> float Container.t = <fun>
# demo (make_stack ());;
Int Element: 17
Int Element: 5
Float Element: 17.100000
Float Element: 5.100000
- : unit = ()
```

The syntactic load here is pretty high, requiring a `let module`
expression to name every intermediate value.  First-class modules are
fairly new to the language, and this is likely to change, but in the
meantime the syntactic load can be pretty daunting.

Let's look a some other more typical examples, where dynamic module
selection is more localized.

_(jyh: This is a rough draft, I'm not sure about the ordering and the
topics, yet.  Switching back to Ron's text now.)

### Dynamically choosing a module

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
run-time based on an environment variable.

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

### Example: A service bundle

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
