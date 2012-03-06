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

The function `compute_bounds` below shows how you can handle errors in
this style.  The function takes a list and a comparison function, and
returns upper and lower bounds for the list by finding the smallest
and largest element on the list.  `List.hd` and `List.last`, which
return `None` when they encounter an empty list, are used to extract
the largest and smallest element of the list.

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

The use of options to encode errors underlines the ambiguity between
errors and other return values.  Indeed, whether you consider not
finding an element in a list or hashtable to be an error depends very
much on context.

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
~~~~~~~~~~~~~~~~~~~~~~~~~~~

without first opening the `Result` module.

### Using `Error`, `Result` and `Or_error`

You have complete freedom in terms of what error type to use with
`Result.t`, but it's often useful to standardize on a single type for
reporting errors, which, among other things, makes it easier to write
small utility functions to automate common error handling patterns,
which we'll see more of below.  

But the question remains, what type should you choose?  It turns out
to be a tricky choice.

Indeed, Core has a distinguished type for this, called `Error.t`.  An
`Error.t` tries to do a number of things well at the same time:

- **Efficient to construct**: in complex systems, many errors may be
  created and noted along the path, but a comparatively small subset
  of those errors will actually be displayed.

### Helper functions


## Exceptions

Exceptions in OCaml are similar to those in mainstream languages like
Java, C# and Python.  In all of these languages, an exception is a way
to interrupt a computation, typically due to some kind of error.
We'll see an exception triggered in OCaml if, for example, we try to
divide an integer by zero:

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
exceptions as well.  Here's an example of a function for looking up
data in an _association list_, _i.e._ a list of key/value pairs.  An
exception is thrown if the key is not found.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# exception Key_not_found of string ;;
exception Key_not_found of string
# let rec find alist key = match alist with
    | [] -> raise (Key_not_found key)
    | (key',data)::tl -> if key = key' then data else find tl key
  ;;
val find : (string * 'a) list -> string -> 'a = <fun>
# let alist = [("a",1); ("b",2)];;
val alist : (string * int) list = [("a", 1); ("b", 2)]
# find alist "a";;
- : int = 1
# find alist "c";;
Exception: Key_not_found "c".
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the above example, the function `raise` is used to terminate the
computation by throwing the exception.  The type of raise may strike
you as somewhat surprising.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# raise;;
- : exn -> 'a = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Having a return type of `'a` where `'a` doesn't come up elsewhere in
the type seems to imply that `raise` returns a value that could be of
any type.  That seems impossible, and it is.  Indeed, functions with
such type signatures necessarily never return at all.  This isn't
restricted to exceptions.  Here's another example of a function that
doesn't return a value.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let rec forever () = forever ();;
val forever : unit -> 'a = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`forever` doesn't return a value for a different reason: it never
terminates.  But the type of `forever`, like that of `raise`, returns
a value of arbitrary type.

The reason this matters is that it means we can call `raise` in any
context, and the return type of `raise` can match the expected type.
Thus, we can throw an exception anywhere in our program, which is the
semantics we want for this kind of error reporting.

### Recovering from exceptions

In the examples so far, exceptions were used to terminate the
execution of the program in question.  But often, we want a program to
actually do something in response to errors, rather than just
exiting.

The core mechanism for handling exceptions in OCaml is the
`try`/`with` statement.  Here's the basic syntax:

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
the next outer exception handler (or terminate the program if there is
none.)

Here's a slightly larger example of how to catch and recover from an
exception using `try`/`with`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
~~~~~~~~~~~~~~~~~~~~~~~~~~~





## Detritus







Here's a simple example of a complete program that uses exceptions.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(* simple_exn.ml *)

open Core.Std

exception Empty_list

let list_max l =
  match l with
  | [] -> raise Empty_list
  | hd :: tl -> List.fold tl ~init:hd ~f:Int.max


let () =
  printf "%d\n" (list_max [1;2;3]);
  printf "%d\n" (list_max []);
  printf "%d\n" (list_max [1;0;-1])
~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we build and then run this example, we'll get the following output:

~~~~~~~~~~~~~~~~~~~~~~~~~~~
$ ./simple_exn
3
Fatal error: exception Simple_exn.Empty_list
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The source of the error is easy enough to figure out by reading the
program.  But outside of toy problems, you typically need more
information to understand how your program wnet wrong.  We can get
this information by turning on display of stack traces by setting an
environment variable:

~~~~~~~~~~~~~~~~~~~~~~~~~~~
$ export OCAMLRUNPARAM=b
$ ./simple_exn
3
Fatal error: exception Simple_exn.Empty_list
Raised at file "simple_exn.ml", line 9, characters 16-26
Called from file "simple_exn.ml", line 15, characters 16-29
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The stack-trace shows multiple points on the call-stack, which helps
track down where in your program the error occurred.

Exceptions can be caught as well as thrown.  We could rewrite the last
section of `simple_exn.ml` to use the OCaml's `try`/`with` construct
so as to produce an error message and continue rather than aborting
execution entirely.  Here, we've created a new function `print_max` to
take care of the error handling once, so we don't need to write it out
explicitly for all three calls.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
let () =
  let print_max l =
    try printf "%d\n" (list_max l)
    with Empty_list -> printf "Empty list\n"
  in
  print_max [1;2;3];
  print_max [];
  print_max [-1;0;1]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we make this change and again run `simple_exn`, we'll see this
output.  Note that we are specifically only catching the `Empty_list`
exception.  If some other exception happens, it will not be caught and
handled here.

~~~~~~~~~~~~~~~~~~~~~~~~~~~
$ ./simple_exn
3
Empty list
1
~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Anatomy of a `try`/`with` clause

Let's dig a little bit into
