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
your return type.  As an example, consider the type of the `find`
function in the list module;

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# List.find;;
- : 'a list -> f:('a -> bool) -> 'a option
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The option in the return type indicates that the function may not
succeed in finding a suitable element, as in these examples:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let x = List.find [1;2;3] ~f:(fun x -> x >= 2) ;;
val x : int option = Some 2
# let y = List.find [1;2;3] ~f:(fun x -> x >= 10) ;;
val y : int option = None
~~~~~~~~~~~~~~~~~~~~~~~~~~~





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
