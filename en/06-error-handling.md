# Error Handling

_(yminsky: this chapter is still very very rough.)_

Nobody likes dealing with errors.  It's tedious, it's easy to get
wrong, and most of the time it's just less fun than thinking about
what happens when your program succeeds.  But error handling is
important, and however much you don't like thinking about it, having
your software fail due to poor error handling code is worse.

Thankfully, OCaml has powerful tools for handling errors reliably and
with a minimum of pain.  In this chapter we'll discuss some of the
different approaches in OCaml to handling errors, and give some advice
on how to design good error-aware interfaces.

We'll start by describinig the two basic approaches for reporting
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
# let x = List.find [1;2;3] ~f:(fun x -> x >= 2) ;;
val x : int option = Some 2
# let y = List.find [1;2;3] ~f:(fun x -> x >= 10) ;;
val y : int option = None
~~~~~~~~~~~~~~~~~~~~~~~~~~~

By making errors explicit in the return values of your functions, you
make it clearer to the caller that there is an error that needs to be
handled. Errors can then be handled explicitly, and either recovered
from or propagated onward.

The function `compute_bounds` is an example of how you can handle
errors using this style.  The function takes a list and a comparison
function, and returns upper and lower bounds for the list by finding
the smallest and largest element on the list.  `List.hd` and
`List.last`, which return `None` when they encounter an empty list,
are used to extract the largest and smallest element of the list.

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

The match statement is used to handle the error cases, propagating any
error in `hd` and `last` into the return value of `compute_bounds`.
On the other hand, in `find_mismatches` below, errors encountered
during the computation do not propagate to the return value of the
function.  The point of `find_mismatches` is to find keys that are
stored in both tables, and so a failure to find a key in one of the
tables actually indicates the lack of an error.

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
error-aware return types let you make an explicit decision how to
handle errors in whatever context you're in.

### Encoding errors with `Result`

Options aren't always a sufficiently expressive way to report errors.
Specifically, when you encode an error as `None`, there's nowhere to
say anything about the nature of the error.

If you need to report more detailed error information, the `Result.t`
type is the way to go.  Here's the definition of the type:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
module Result : sig
   type ('a,'b) t = | Ok of 'a
                    | Error of 'b
end
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`Result` is nothing more than an option augmented with the ability to
store other information in the error case.  That said, it's a very
important and often-used type, so much so that the constructors `Ok`
and `Error` are promoted to the top-level by `Core.Std`, much like
`Some` and `None`.  As such, we can write:

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

The type `'a Or_error.t` is just a shorthand for `('a,Error.t)
Result.t`, and it is, after `option`, the most common way of returning
errors in Core.

### Standard error-handling idioms

When you're dealing with a small number of operations that might
return errors, then handling the errors explicitly using `match`
statements is a good way to go.  But as you do more and more error
handling, you'll find some useful idioms that come up over and over
again.  A number of these have already been codified in the interfaces
of modules like `Option` and `Result`.  One particularly useful one
has to do with the function `bind`, which is also written using the
following infix operator.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
val (>>=) : 'a option -> ('a -> 'b option) -> 'b option
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`bind` is a way of sequencing together error-producing functions in
such a way that the first one to actually produce an error terminates
the computation.  We can use this operator to rewrite the
`compute_bounds` function we wrote earlier in the chapter.  We locally
open the `Option.Monad_infix` module to get access to the infix
operators.  (It's called `Monad_infix` because the bind operator is
technically part of a structure called a `Monad`, which we'll talk
about more in chapter {{{ASYNC}}}.)

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let compute_bounds ~cmp list =
    let open Option.Monad_infix in
    let sorted = List.sort ~cmp list in
    List.hd sorted >>= (fun first ->
      List.last sorted >>= (fun last ->
        Some (first,last)))
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a bit easier to read if we write it with fewer parens and less
indentation, as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let compute_bounds ~cmp list =
    let open Option.Monad_infix in
    let sorted = List.sort ~cmp list in
    List.hd sorted   >>= fun first ->
    List.last sorted >>= fun last  ->
    Some (first,last)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are other useful functions, such as `Option.both`, which given
two optional values, produces a new optional pair that is `None` if
either of its arguments are `None`.  We can further shorten
`compute_bounds` using that as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let compute_bounds ~cmp list =
    let sorted = List.sort ~cmp list in
    Option.both (List.hd sorted) (List.last sorted)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

What's great about these error-handling functions is that they let you
express your error handling both explicitly and concisely.  Our
examples all used options, but similar functionality is available for
`Result` and `Or_error`.

## Exceptions

Exceptions in OCaml are similar to those in mainstream languages like
Java, C# and Python.  In all of these languages, an exception is a way
to interrupt a computation and report an error.  We'll see an
exception triggered in OCaml if, for example, we try to divide an
integer by zero:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# 3 / 0;;
Exception: Division_by_zero.
~~~~~~~~~~~~~~~~~~~~~~~~~~~

And an exception can terminate a computation even if it happens nested
a few levels deep in a computation.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# List.map ~f:(fun x -> 100 / x) [1;3;0;4]
  |! List.reduce_exn ~f:(+) ;;
Exception: Division_by_zero.
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`Divide_by_zero` is a built-in exception, but you can define your own
exceptions as well.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# exception Key_not_found of string;;
exception Key_not_found of string
# Key_not_found "a";;
- : exn = Key_not_found("a")
~~~~~~~~~~~~~~~~~~~~~~~~~~~

As you can see, exceptions are first-class values with their own type,
`exn`.  We can use this exception to report errors, for example, in
the following function for looking up a key in an _association list_,
_i.e._ a list of key/value pairs.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let rec find alist key = match alist with
    | [] -> raise (Key_not_found key)
    | (key',data) :: tl -> if key = key' then data else find tl key
  ;;
val find : (string * 'a) list -> string -> 'a = <fun>
# let alist = [("a",1); ("b",2)];;
val alist : (string * int) list = [("a", 1); ("b", 2)]
# find alist "a";;
- : int = 1
# find alist "c";;
Exception: Key_not_found("c").
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the above example, the function `raise` throws the exception, thus
terminating the type of the computation.  The type of raise is a bit
surprising when you first see it:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# raise;;
- : exn -> 'a = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Having a return type of an otherwise unused `'a` is surprising in that
it suggests that `raise` could return a value of any type.  That seems
impossible, and it is.  The reason `raise` has this type is that it
never returns at all. This isn't restricted to exceptions.  Here's
another example of a function that doesn't return a value.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let rec forever () = forever ();;
val forever : unit -> 'a = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`forever` doesn't return a value for a different reason: it never
terminates.

This all matters because it means that the return type of `raise` can
match the expected type, whatever the context.  Thus, we can in a
type-safe way throw an exception anywhere in our program.

### Handling exceptions using `try`/`with`

In the examples so far, exceptions were used to terminate the
execution of the computation in question.  But often, we want a
program to respond to an error in some way other than simply exiting.

The core mechanism for handling exceptions in OCaml is the
`try`/`with` statement.  Here's the basic syntax.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-syntax }
try <expr> with
| <pat1> -> <expr1>
| <pat2> -> <expr2>
...
| <patk> -> <exprk>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Such a `try`/`with` clause would evaluate `<expr>`, and if the
evaluation completes without returning an exception, then the value of
the overall expression is the value of `<expr>`.

But if evaluating `<expr>` leads to an exception being thrown, then
the exception will be fed to the pattern match statements following
`with`.  If the exception matches a pattern, then the expression on
the right hand side of that pattern will be evaluated.  Otherwise, the
original exception continues up the call stack, and will be caught at
the next outer exception handler, or terminate the program if there is
none.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
~~~~~~~~~~~~~~~~~~~~~~~~~~~

_(more here...)_

### From exceptions to error-aware types and back again ###

Both exceptions and error-aware types are necessary parts of
programming in OCaml.  As such, it often makes sense to move between
the two worlds.  Happily, Core comes with some useful helper functions
to help you do just that.
