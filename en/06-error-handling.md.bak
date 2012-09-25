# Error Handling

Nobody likes dealing with errors.  It's tedious, it's easy to get
wrong, and it's usually just not as fun as planning out how your
program is going to succeed.  But error handling is important, and
however much you don't like thinking about it, having your software
fail due to poor error handling code is worse.

Thankfully, OCaml has powerful tools for handling errors reliably and
with a minimum of pain.  In this chapter we'll discuss some of the
different approaches in OCaml to handling errors, and give some advice
on how to design interfaces that help rather than hinder error
handling.

We'll start by describing the two basic approaches for reporting
errors in OCaml: error-aware return types and exceptions.

## Error-aware return types

The best way in OCaml to signal an error is to include that error in
your return value.  Consider the type of the `find` function in the
list module.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# List.find;;
- : 'a list -> f:('a -> bool) -> 'a option
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The option in the return type indicates that the function may not
succeed in finding a suitable element, as you can see below.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# List.find [1;2;3] ~f:(fun x -> x >= 2) ;;
- : int option = Some 2
# List.find [1;2;3] ~f:(fun x -> x >= 10) ;;
- : int option = None
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Having errors be explicit in the return values of your functions tells
the caller that there is an error that needs to be handled. The caller
can then handle the error explicitly, either recovering from the error
or propagating it onward.

The function `compute_bounds` below is an example of how you can
handle errors in this style.  The function takes a list and a
comparison function, and returns upper and lower bounds for the list
by finding the smallest and largest element on the list.  `List.hd`
and `List.last`, which return `None` when they encounter an empty
list, are used to extract the largest and smallest element of the
list.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let compute_bounds ~cmp list =
    let sorted = List.sort ~cmp list in
    let smallest = List.hd sorted in
    let largest = List.last sorted in
    match smallest, largest with
    | None,_ | _, None -> None
    | Some x, Some y -> Some (x,y)
  ;;
val compute_bounds :
  cmp:('a -> 'a -> int) -> 'a list -> ('a * 'a) option = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The match statement is used to handle the error cases, propagating an
error in `hd` or `last` into the return value of `compute_bounds`.  On
the other hand, in `find_mismatches` below, errors encountered during
the computation do not propagate to the return value of the function.
`find_mismatches` takes two hashtables as its arugments and tries to
find keys that are stored in both.  As such, a failure to find a key
in one of the tables isn't really an error.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let find_mismatches table1 table2 =
     Hashtbl.fold table1 ~init:[] ~f:(fun ~key ~data errors ->
        match Hashtbl.find table2 key with
        | Some data' when data' <> data -> key :: errors
        | _ -> errors
     )
 ;;
val find_mismatches :
  ('a, 'b) Hashtbl.t -> ('a, 'b) Hashtbl.t -> 'a list = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The use of options to encode errors underlines the fact that it's not
clear whether a particular outcome, like not finding something on a
list, is really an error, or just another valid outcome of your
function.  This turns out to be very context-dependent, and
error-aware return types give you a uniform way of handling the result
that works well for both situations.

### Encoding errors with `Result`

Options aren't always a sufficiently expressive way to report errors.
Specifically, when you encode an error as `None`, there's nowhere to
say anything about the nature of the error.

`Result.t` is meant to address this deficiency.  Here's the
definition:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
module Result : sig
   type ('a,'b) t = | Ok of 'a
                    | Error of 'b
end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A `Result.t` is essentially an option augmented with the ability to
store other information in the error case.  Like `Some` and `None` for
options, the constructors `Ok` and `Error` are promoted to the
top-level by `Core.Std`.  As such, we can write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# [ Ok 3; Error "abject failure"; Ok 4 ];;
[Ok 3; Error "abject failure"; Ok 4]
- : (int, string) Result.t list =
[Ok 3; Error "abject failure"; Ok 4]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

without first opening the `Result` module.

### `Error` and `Or_error`

`Result.t` gives you complete freedom to choose the type of value you
use to represent errors, but it's often useful to standardize on an
error type.  Among other things, this makes it easier to write utility
functions to automate common error handling patterns.

But which type to choose?  Is it better to represent errors as
strings?  Or S-expressions?  Or something else entirely?

Core's answer to this question is the `Error.t` type, which tries to
forge a good compromise between efficiency, convenience and control
over the presentation of errors.

It might not be obvious at first why efficiency is an issue at all.
But generating error messages is an expensive business.  An ASCII
representation of a type can be quite time-consuming to construct,
particularly if it includes expensive-to-convert numerical datatypes.

`Error` gets around this issue through laziness.  In particular, an
`Error.t` allows you to put off generation of the actual error string
until you actually need, which means a lot of the time you never have
to construct it at all. You can of course construct an error directly
from a string:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Error.of_string "something went wrong";;
- : Core.Std.Error.t = "something went wrong"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A more interesting construction message from a performance point of
view is to construct an `Error.t` from a thunk:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Error.of_thunk (fun () ->
    sprintf "something went wrong: %f" 32.3343);;
  - : Core.Std.Error.t = "something went wrong: 32.334300"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this case, we can benefit from the laziness of `Error`, since the
thunk won't be called until the `Error.t` is converted to a string.

We can also create an `Error.t` based on an s-expression converter.
This is probably the most common idiom in Core.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Error.create "Something failed a long time ago" Time.epoch Time.sexp_of_t;;
- : Core.Std.Error.t =
"Something failed a long time ago: (1969-12-31 19:00:00.000000)"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here, the value `Time.epoch` is included in the error, but
`Time.sexp_of_t`, which is used for converting the time to an
s-expression, isn't run until the error is converted to a string.
Using the Sexplib syntax-extension, which is discussed in more detail
in chapter {{SYNTAX}}, we can inline create an s-expression converter
for a collection of types, thus allowing us to register multiple
pieces of data in an `Error.t`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Error.create "Something went terribly wrong"
    (3.5, ["a";"b";"c"],6034)
    <:sexp_of<float * string list * int>> ;;
- : Core.Std.Error.t = "Something went terribly wrong: (3.5(a b c)6034)"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here, the declaration `<:sexp_of<float * string list * int>>` asks
Sexplib to generate the sexp-converter for the tuple.

Error also has operations for transforming errors.  For example, it's
often useful to augment an error with some extra information about the
context of the error, or to combine multipler errors together.
`Error.of_list` and `Error.tag` fill these roles.

The type `'a Or_error.t` is just a shorthand for `('a,Error.t)
Result.t`, and it is, after `option`, the most common way of returning
errors in Core.

### `bind` and other error-handling idioms

As you write more error handling code, you'll discover that certain
patterns start to emerge.  A number of these common patterns been
codified in the interfaces of modules like `Option` and `Result`.  One
particularly useful one is built around the function `bind`, which is
both an ordinary function and an infix operator `>>=`, both with the
same type signature:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
val (>>=) : 'a option -> ('a -> 'b option) -> 'b option
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`bind` is a way of sequencing together error-producing functions so
that that the first one to produce an error terminates the
computation.  In particular, `None >>= f` returns `None` without
calling `f`, and `Some x >>= f` returns `f x`.  We can use a nested
sequence of these binds to express a multi-stage computation that can
fail at any stage.  Here's a rewrite `compute_bounds` in this style.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let compute_bounds ~cmp list =
    let open Option.Monad_infix in
    let sorted = List.sort ~cmp list in
    List.hd sorted >>= (fun first ->
      List.last sorted >>= (fun last ->
        Some (first,last)))
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that we locally open the `Option.Monad_infix` module to get
access to the infix operator `>>=`.  The module is called
`Monad_infix` because the bind operator is part of a sub-interface
called `Monad`, which we'll talk about more in chapter {{{ASYNC}}}.

This is a bit easier to read if we write it with fewer parentheses and
less indentation, as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let compute_bounds ~cmp list =
    let open Option.Monad_infix in
    let sorted = List.sort ~cmp list in
    List.hd sorted   >>= fun first ->
    List.last sorted >>= fun last  ->
    Some (first,last)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are other usuful idioms encoded in the functions in `Option`.
Another example is `Option.both`, which takes two optional values and
produces a new optional pair that is `None` if either of its arguments
are `None`.  Using `Option.both`, we can make `compute_bounds` even
shorter.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let compute_bounds ~cmp list =
    let sorted = List.sort ~cmp list in
    Option.both (List.hd sorted) (List.last sorted)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

These error-handling functions are valuable because they let you
express your error handling both explicitly and concisely.  We've only
discussed these functions in the context of the `Option` module, but
similar functionality is available in both `Result` and `Or_error`.

## Exceptions

Exceptions in OCaml are not that different from exceptions in many
other languages, like Java, C# and Python.  In all these cases,
exceptions are a way to terminate a computation and report an error,
while providing a mechanism to catch and handle (and possibly recover
from) exceptions that are triggered by sub-computations.

We'll see an exception triggered in OCaml if, for example, we try to
divide an integer by zero:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# 3 / 0;;
Exception: Division_by_zero.
~~~~~~~~~~~~~~~~~~~~~~~~~~~

And an exception can terminate a computation even if it happens nested
a few levels deep in a computation.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# List.map ~f:(fun x -> 100 / x) [1;3;0;4];;
Exception: Division_by_zero.
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In addition to built-in exceptions like `Divide_by_zero`, OCaml lets
you define your own.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# exception Key_not_found of string;;
exception Key_not_found of string
# Key_not_found "a";;
- : exn = Key_not_found("a")
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here's an example of a function for looking up a key in an
_association list_, _i.e._ a list of key/value pairs which uses this
newly-defined exception:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let rec find_exn alist key = match alist with
    | [] -> raise (Key_not_found key)
    | (key',data) :: tl -> if key = key' then data else find_exn tl key
  ;;
val find_exn : (string * 'a) list -> string -> 'a = <fun>
# let alist = [("a",1); ("b",2)];;
val alist : (string * int) list = [("a", 1); ("b", 2)]
# find_exn alist "a";;
- : int = 1
# find_exn alist "c";;
Exception: Key_not_found("c").
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that we named the function `find_exn` to warn the user that the
function routinely throws exceptions, a convention that is used
heavily in Core.

In the above example, `raise` throws the exception, thus terminating
the computation.  The type of raise is a bit surprising when you first
see it:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# raise;;
- : exn -> 'a = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Having the return type be an otherwise unused type variable `'a`
suggests that `raise` could return a value of any type.  That seems
impossible, and it is.  `raise` has this type because it never returns
at all. This behavior isn't restricted to functions like `raise` that
terminate by throwing exceptions.  Here's another example of a
function that doesn't return a value.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let rec forever () = forever ();;
val forever : unit -> 'a = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`forever` doesn't return a value for a different reason: it is an
infinite loop.

This all matters because it means that the return type of `raise` can
be whatever it needs to be to fit in to the context it is called in.
Thus, the type system will let us throw an exception anywhere in a
program.

<sidebar>
<title>Declaring exceptions with `with sexp`</title>

OCaml can't always generate a useful textual representation of your
exception, for example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# exception Wrong_date of Date.t;;
exception Wrong_date of Date.t
# Wrong_date (Date.of_string "2011-02-23");;
- : exn = Wrong_date(_)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

But if you declare the exception using `with sexp` (and the
constituent types have sexp converters), we'll get something with more
information.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# exception Wrong_date of Date.t with sexp;;
exception Wrong_date of Core.Std.Date.t
# Wrong_date (Date.of_string "2011-02-23");;
- : exn = (.Wrong_date 2011-02-23)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The period in front of `Wrong_date` is there because the
representation generated by `with sexp` includes the full module path
of the module where the exception in question is defined.  This is
quite useful in tracking down which precise exception is being
reported.  In this case, since we've declared the exception at the
toplevel, that module path is trivial.

</sidebar>

### Exception handlers

So far, we've only seen exceptions fully terminate the execution of a
computation.  But often, we want a program to be able to respond to
and recover from an exception.  This is achieved through the use of
_exception handlers_.

In OCaml, an exception handler is declared using a `try`/`with`
statement.  Here's the basic syntax.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-syntax }
try <expr> with
| <pat1> -> <expr1>
| <pat2> -> <expr2>
...
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A `try`/`with` clause would first evaluate `<expr>`, and if that
evaluation completes without returning an exception, then the value of
the overall expression is the value of `<expr>`.

But if evaluating `<expr>` leads to an exception being thrown, then
the exception will be fed to the pattern match statements following
the `with`.  If the exception matches a pattern, then the expression
on the right hand side of that pattern will be evaluated.  Otherwise,
the original exception continues up the call stack, to be handled by
the next outer exception handler, or terminate the program if there is
none.

### Cleaning up in the presence of exceptions

One headache with exceptions is that they can terminate your execution
at unexpected places, leaving your program in an awkward state.
Consider the following code snippet:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
let load_config filename =
  let inc = In_channel.create filename in
  let config = Config.t_of_sexp (Sexp.input_sexp inc) in
  In_channel.close inc;
  config
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The problem with this code is that the function that loads the
s-expression and parses it into a `Config.t` might thrown an exception
if the config file in question is malformed.  Unfortunately, that
means that the `In_channel.t` that was opened will never be closed,
leading to a file-descriptor leak.

We can fix this using Core's `protect` function.  The basic purpose of
`protect` is to ensure that the `finally` thunk will be called when
`f` exits, whether it exited normally or with an exception.  Here's
how it could be used to fix `load_config`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
let load_config filename =
  let inc = In_channel.create filename in
  protect ~f:(fun () -> Config.t_of_sexp (Sexp.input_sexp inc)
    ~finally:(fun () -> In_channel.close inc)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Catching specific exceptions

OCaml's exception-handling system allows you to tune your
error-recovery logic to the particular error that was thrown.  For
example, `List.find_exn` always throws `Not_found`.  You can take
advantage of this in your code, for example, let's define a function
called `lookup_weight`, with the following signature:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(** [lookup_weight ~compute_weight alist key] Looks up a
    floating-point weight by applying [compute_weight] to the data
    associated with [key] by [alist].  If [key] is not found, then
    return 0.
*)
val lookup_weight :
  compute_weight:('data -> float) -> ('key * 'data) list -> 'key -> float
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can implement such a function using exceptions as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let lookup_weight ~compute_weight alist key =
    try
      let data = List.Assoc.find_exn alist key in
      compute_weight data
    with
      Not_found -> 0. ;;
val lookup_weight :
  compute_weight:('a -> float) -> ('b * 'a) list -> 'b -> float =
  <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This implementation is more problematic than it looks.  In particular,
what happens if `compute_weight` itself throws an exception?  Ideally,
`lookup_weight` should propagate that exception on, but if the
exception happens to be `Not_found`, then that's not what will happen:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# lookup_weight ~compute_weight:(fun _ -> raise Not_found)
    ["a",3; "b",4] "a" ;;
- : float = 0.
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This kind of problem is hard to detect in advance, because the type
system doesn't tell us what kinds of exceptions a given function might
throw.  Because of this kind of confusion, it's usually better to
avoid catching specific exceptions.  In this case, we can improve the
code by catching the exception in a narrower scope.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let lookup_weight ~compute_weight alist key =
    match
      try Some (List.Assoc.find_exn alist key) with
      | Not_found -> None
    with
    | None -> 0.
    | Some data -> compute_weight data ;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~

At which point, it makes sense to simply use the non-exception
throwing function, `List.Assoc.find`, instead.

### Backtraces

A big part of the point of exceptions is to give useful debugging
information.  But at first glance, OCaml's exceptions can be less than
informative.   Consider the following simple program.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* exn.ml *)

open Core.Std
exception Empty_list

let list_max = function
  | [] -> raise Empty_list
  | hd :: tl -> List.fold tl ~init:hd ~f:(Int.max)

let () =
  printf "%d\n" (list_max [1;2;3]);
  printf "%d\n" (list_max [])
~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we build and run this program, we'll get a pretty uninformative
error:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .shell }
$ ./exn
3
Fatal error: exception Exn.Empty_list
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The example in qeustion is short enough that it's quite easy to see
where the error came from.  But in a complex program, simply knowing
which exception was thrown is usually not enough information to figure
out what went wrong.

We can get more information from OCaml if we turn on stack traces.
This can be done by setting the `OCAMLRUNPARAM` environment variable,
as shown:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .shell }
exn $ export OCAMLRUNPARAM=b
exn $ ./exn
3
Fatal error: exception Exn.Empty_list
Raised at file "exn.ml", line 7, characters 16-26
Called from file "exn.ml", line 12, characters 17-28
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Backtraces can also be obtained at runtime.  In particular,
`Exn.backtrace` will return the backtrace fo the most recently thrown
exception.

### Exceptions for control flow

### From exceptions to error-aware types and back again ###

Both exceptions and error-aware types are necessary parts of
programming in OCaml.  As such, you often need to move between these
two worlds.  Happily, Core comes with some useful helper functions to
help you do just that.  For example, given a piece of code that can
throw an exception, you can capture that exception into an option as
follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let find alist key =
    Option.try_with (fun () -> find_exn alist key) ;;
val find : (string * 'a) list -> string -> 'a option = <fun>
# find ["a",1; "b",2] "c";;
- : int Core.Std.Option.t = None
# find ["a",1; "b",2] "b";;
- : int Core.Std.Option.t = Some 2
~~~~~~~~~~~~~~~~~~~~~~~~~~~

And `Result` and `Or_error` have similar `try_with` functions.  So, we
could write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let find alist key =
    Result.try_with (fun () -> find_exn alist key) ;;
val find : (string * 'a) list -> string -> ('a, exn) Result.t = <fun>
# find ["a",1; "b",2] "c";;
- : (int, exn) Result.t = Result.Error Key_not_found("c")
~~~~~~~~~~~~~~~~~~~~~~~~~~~

And then we can re-raise that exception:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Result.ok_exn (find ["a",1; "b",2] "b");;
- : int = 2
# Result.ok_exn (find ["a",1; "b",2] "c");;
Exception: Key_not_found("c").
~~~~~~~~~~~~~~~~~~~~~~~~~~~
