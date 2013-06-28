# Error Handling

Nobody likes dealing with errors.  It's tedious, it's easy to get
wrong, and it's usually just not as fun as planning out how your
program is going to succeed.  But error handling is important, and
however much you don't like thinking about it, having your software
fail due to poor error handling code is worse.

Thankfully, OCaml has powerful tools for handling errors reliably and
with a minimum of pain.  In this chapter we'll discuss some of the
different approaches in OCaml to handling errors, and give some advice
on how to design interfaces that make error handling easier.

We'll start by describing the two basic approaches for reporting
errors in OCaml: error-aware return types and exceptions.

## Error-aware return types

The best way in OCaml to signal an error is to include that error in
your return value.  Consider the type of the `find` function in the
`List` module.

```ocaml
# List.find;;
- : 'a list -> f:('a -> bool) -> 'a option
```

The option in the return type indicates that the function may not
succeed in finding a suitable element, as you can see below.

```ocaml
# List.find [1;2;3] ~f:(fun x -> x >= 2) ;;
- : int option = Some 2
# List.find [1;2;3] ~f:(fun x -> x >= 10) ;;
- : int option = None
```

Having errors be explicit in the return values of your functions tells
the caller that there is an error that needs to be handled.  The
caller can then handle the error explicitly, either recovering from
the error or propagating it onward.

Consider the `compute_bounds` function defined below.  The function
takes a list and a comparison function, and returns upper and lower
bounds for the list by finding the smallest and largest element on the
list.  `List.hd` and `List.last`, which return `None` when they
encounter an empty list, are used to extract the largest and smallest
element of the list.

```ocaml
# let compute_bounds ~cmp list =
    let sorted = List.sort ~cmp list in
    match List.hd sorted, List.last sorted with
    | None,_ | _, None -> None
    | Some x, Some y -> Some (x,y)
  ;;
val compute_bounds :
  cmp:('a -> 'a -> int) -> 'a list -> ('a * 'a) option = <fun>
```

The match statement is used to handle the error cases, propagating a
None in `hd` or `last` into the return value of `compute_bounds`.

On the other hand, in `find_mismatches` below, errors encountered
during the computation do not propagate to the return value of the
function.  `find_mismatches` takes two hash tables as arguments, and
searches for keys that have different data in one table than in the
other.  As such, the failure to find a key in one table isn't a
failure of any sort.

```ocaml
# let find_mismatches table1 table2 =
     Hashtbl.fold table1 ~init:[] ~f:(fun ~key ~data mismatches ->
        match Hashtbl.find table2 key with
        | Some data' when data' <> data -> key :: mismatches
        | _ -> mismatches
     )
 ;;
val find_mismatches :
  ('a, 'b) Hashtbl.t -> ('a, 'b) Hashtbl.t -> 'a list = <fun>
```

The use of options to encode errors underlines the fact that it's not
clear whether a particular outcome, like not finding something on a
list, is an error or is just another valid outcome.  This depends on
the larger context of your program, and thus is not something that a
general purpose library can know in advance.  One of the advantages of
error-aware return types is that they work well in both situations.

### Encoding errors with `Result`

Options aren't always a sufficiently expressive way to report errors.
Specifically, when you encode an error as `None`, there's nowhere to
say anything about the nature of the error.

`Result.t` is meant to address this deficiency.  The type is defined
as follows.

```ocaml
module Result : sig
   type ('a,'b) t = | Ok of 'a
                    | Error of 'b
end
```

A `Result.t` is essentially an option augmented with the ability to
store other information in the error case.  Like `Some` and `None` for
options, the constructors `Ok` and `Error` are promoted to the
toplevel by `Core.Std`.  As such, we can write:

```ocaml
# [ Ok 3; Error "abject failure"; Ok 4 ];;
- : (int, string) Core.Result.t list = [Ok 3; Error "abject failure"; Ok 4]
```

without first opening the `Result` module.

### `Error` and `Or_error`

`Result.t` gives you complete freedom to choose the type of value you
use to represent errors, but it's often useful to standardize on an
error type.  Among other things, this makes it easier to write utility
functions to automate common error handling patterns.

But which type to choose?  Is it better to represent errors as
strings?  Some more structured representation like XML?  Or something
else entirely?

Core's answer to this question is the `Error.t` type, which tries to
forge a good compromise between efficiency, convenience, and control
over the presentation of errors.

It might not be obvious at first why efficiency is an issue at all.
But generating error messages is an expensive business.  An ASCII
representation of a value can be quite time-consuming to construct,
particularly if it includes expensive-to-convert numerical data.

`Error` gets around this issue through laziness.  In particular, an
`Error.t` allows you to put off generation of the error string until
and unless you need it, which means a lot of the time you never have
to construct it at all.  You can of course construct an error directly
from a string:

```ocaml
# Error.of_string "something went wrong";;
- : Core.Std.Error.t = "something went wrong"
```

But you can also construct an `Error.t` from a _thunk_, _i.e._, a
function that takes a single argument of type `unit`.

```ocaml
# Error.of_thunk (fun () ->
    sprintf "something went wrong: %f" 32.3343);;
  - : Core.Std.Error.t = "something went wrong: 32.334300"
```

In this case, we can benefit from the laziness of `Error`, since the
thunk won't be called unless the `Error.t` is converted to a string.

The most common way to create `Error.t`s is using _s-expressions_.  An
s-expression is a balanced parenthetical expression where the leaves
of the expressions are strings.  Thus, the following is a simple
s-expression:

```
(This (is an) (s expression))
```

S-expressions are supported by the Sexplib library that is distributed
with Core, and is the most common serialization format used in Core.
Indeed, most types in Core come with built-in s-expression converters.
Here's an example of creating an error using the sexp converter for
times, `Time.sexp_of_t`.

```ocaml
# Error.create "Something failed a long time ago" Time.epoch Time.sexp_of_t;;
- : Core.Std.Error.t =
"Something failed a long time ago: (1969-12-31 19:00:00.000000)"
```

Note that the time isn't actually serialized into an s-expression
until the error is printed out.  We're not restricted to doing this
kind of error reporting with built-in types.  This will be discussed
in more detail in [xref](#data-serialization-with-s-expressions), but
Sexplib comes with a language extension that can auto-generate
sexp-converters for newly generated types, as shown below.

```ocaml
# let custom_to_sexp = <:sexp_of<float * string list * int>>;;
val custom_to_sexp : float * string list * int -> Sexp.t = <fun>
# custom_to_sexp (3.5, ["a";"b";"c"], 6034);;
- : Sexp.t = (3.5 (a b c) 6034)
```

We can use this same idiom for generating an error.

```ocaml
# Error.create "Something went terribly wrong"
    (3.5, ["a";"b";"c"], 6034)
    <:sexp_of<float * string list * int>> ;;
- : Error.t = Something went terribly wrong: (3.5(a b c)6034)
```

`Error` also supports operations for transforming errors.  For
example, it's often useful to augment an error with some extra
information about the context of the error or to combine multiple
errors together.  `Error.tag` and `Error.of_list` fulfill these roles,
as you can see below.

The type `'a Or_error.t` is just a shorthand for `('a,Error.t)
Result.t`, and it is, after `option`, the most common way of returning
errors in Core.

### `bind` and other error-handling idioms

As you write more error handling code in OCaml, you'll discover that
certain patterns start to emerge.  A number of these common patterns
have been codified by functions in modules like `Option` and `Result`.
One particularly useful pattern is built around the function `bind`,
which is both an ordinary function and an infix operator `>>=`.
Here's the definition of `bind` for options.

```ocaml
# let bind option f =
    match option with
    | None -> None
    | Some x -> f x
  ;;
val bind : 'a option -> ('a -> 'b option) -> 'b option = <fun>
```

As you can see, `bind None f` returns `None` without calling `f`, and
`bind (Some x) f` returns `f x`.  Perhaps surprisingly, `bind` can be
used as a way of sequencing together error-producing functions so that
the first one to produce an error terminates the computation.  Here's
a rewrite of `compute_bounds` to use a nested series of `bind`s.

```ocaml
# let compute_bounds ~cmp list =
    let sorted = List.sort ~cmp list in
    Option.bind (List.hd sorted) (fun first ->
      Option.bind (List.last sorted) (fun last ->
        Some (first,last)))
  ;;
val compute_bounds : cmp:('a -> 'a -> int) -> 'a list -> ('a * 'a) option =
  <fun>
```

The above code is a little bit hard to swallow, however, on a
syntactic level.  We can make it easier to read, and drop some of the
parentheses, by using the infix operator form of bind, which we get
access to by locally opening `Option.Monad_infix`.  The module is
called `Monad_infix` because the bind operator is part of a
sub-interface called `Monad`, which we'll talk about more in
[xref](#concurrent-programming-with-async).

```ocaml
# let compute_bounds ~cmp list =
    let open Option.Monad_infix in
    let sorted = List.sort ~cmp list in
    List.hd sorted   >>= fun first ->
    List.last sorted >>= fun last  ->
    Some (first,last)
  ;;
val compute_bounds : cmp:('a -> 'a -> int) -> 'a list -> ('a * 'a) option =
  <fun>
```

This use of `bind` isn't really materially better than the one we
started with, and indeed, for small examples like this, direct
matching of options is generally better than using `bind`.  But for
large complex examples with many stages of error-handling, the bind
idiom becomes clearer and easier to manage.

There are other useful idioms encoded in the functions in `Option`.
One example is `Option.both`, which takes two optional values and
produces a new optional pair that is `None` if either of its arguments
are `None`.  Using `Option.both`, we can make `compute_bounds` even
shorter.

```ocaml
# let compute_bounds ~cmp list =
    let sorted = List.sort ~cmp list in
    Option.both (List.hd sorted) (List.last sorted)
  ;;
val compute_bounds : cmp:('a -> 'a -> int) -> 'a list -> ('a * 'a) option =
  <fun>
```

These error-handling functions are valuable because they let you
express your error handling both explicitly and concisely.  We've only
discussed these functions in the context of the `Option` module, but
similar functionality is available in both `Result` and `Or_error`.

## Exceptions

Exceptions in OCaml are not that different from exceptions in many
other languages, like Java, C# and Python.  Exceptions are a way to
terminate a computation and report an error, while providing a
mechanism to catch and handle (and possibly recover from) exceptions
that are triggered by sub-computations.

You can trigger an exception by, for example, dividing an integer by
zero:

```ocaml
# 3 / 0;;
Exception: Division_by_zero.
```

And an exception can terminate a computation even if it happens nested
somewhere deep within it.

```ocaml
# List.map ~f:(fun x -> 100 / x) [1;3;0;4];;
Exception: Division_by_zero.
```

If we put a `printf` in the middle of the computation, we can see that
`List.map` is interrupted part way through it's execution, never
getting to the end of the list.


```ocaml
# List.map ~f:(fun x -> printf "%d\n%!" x; 100 / x) [1;3;0;4];;
1
3
0
Exception: Division_by_zero.
```

In addition to built-in exceptions like `Divide_by_zero`, OCaml lets
you define your own.

```ocaml
# exception Key_not_found of string;;
exception Key_not_found of string
# raise (Key_not_found "a");;
Exception: Key_not_found("a").
```

Here's an example of a function for looking up a key in an
_association list_, _i.e._ a list of key/value pairs which uses this
newly-defined exception:

```ocaml
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
```

Note that we named the function `find_exn` to warn the user that the
function routinely throws exceptions, a convention that is used
heavily in Core.

In the above example, `raise` throws the exception, thus terminating
the computation.  The type of raise is a bit surprising when you first
see it:

```ocaml
# raise;;
- : exn -> 'a = <fun>
```

The return type of `'a` suggests that `raise` could return a value of
any type.  That seems impossible, and it is.  Really, `raise` has this
type because it never returns at all. This behavior isn't restricted
to functions like `raise` that terminate by throwing exceptions.
Here's another example of a function that doesn't return a value.

```ocaml
# let rec forever () = forever ();;
val forever : unit -> 'a = <fun>
```

`forever` doesn't return a value for a different reason: it is an
infinite loop.

This all matters because it means that the return type of `raise` can
be whatever it needs to be to fit in to the context it is called in.
Thus, the type system will let us throw an exception anywhere in a
program.

<note>
<title>Declaring exceptions with `with sexp`</title>

OCaml can't always generate a useful textual representation of an
exception.  For example:

```ocaml
# exception Wrong_date of Date.t;;
exception Wrong_date of Date.t
# Wrong_date (Date.of_string "2011-02-23");;
- : exn = Wrong_date(_)
```

But if we declare the exception using `with sexp` (and the constituent
types have sexp converters), we'll get something with more
information.

```ocaml
# exception Wrong_date of Date.t with sexp;;
exception Wrong_date of Core.Std.Date.t
# Wrong_date (Date.of_string "2011-02-23");;
- : exn = (.Wrong_date 2011-02-23)
```

The period in front of `Wrong_date` is there because the
representation generated by `with sexp` includes the full module path
of the module where the exception in question is defined.  In this
case, since we've declared the exception at the toplevel, that module
path is trivial.

This is all part of the support for s-expressions provided by the
Sexplib library and syntax-extension, which is described in more
detail in [xref](#data-serialization-with-s-expressions).

</note>

### Helper functions for throwing exceptions

OCaml and Core provide a number of helper functions to simplify the
task of throwing exceptions.  The simplest one is `failwith`, which
could be defined as follows:

```ocaml
# let failwith msg = raise (Failure msg);;
val failwith : string -> 'a = <fun>
```

There are several other useful functions for raising exceptions, which
can be found in the API documentation for the `Common` and `Exn`
modules in Core.

Another important way of throwing an exception is the `assert`
directive.  `assert` is used for situations where a violation of the
condition in question indicates a bug.  Consider the following piece
of code for zipping together two lists.

```ocaml
# let merge_lists xs ys ~f =
    if List.length xs <> List.length ys then None
    else
      let rec loop xs ys =
        match xs,ys with
        | [],[] -> []
        | x::xs, y::ys -> f x y :: loop xs ys
        | _ -> assert false
      in
      Some (loop xs ys)
   ;;
 val merge_lists : 'a list -> 'b list -> f:('a -> 'b -> 'c) -> 'c list option =
  <fun>
# merge_lists [1;2;3] [-1;1;2] ~f:(+);;
- : int list option = Some [0; 3; 5]
# merge_lists [1;2;3] [-1;1] ~f:(+);;
- : int list option = None
```

Here we use `assert false`, which means that the assert is guaranteed
to trigger.  In general, one can put an arbitrary condition in the
assertion.

In this case, the assert can never be triggered because we have a
check that makes sure that the lists are of the same length before we
call `loop`.  If we change the code so that we drop this test, then we
can trigger the assert.

```ocaml
# let merge_lists xs ys ~f =
      let rec loop xs ys =
        match xs,ys with
        | [],[] -> []
        | x::xs, y::ys -> f x y :: loop xs ys
        | _ -> assert false
      in
      loop xs ys
   ;;
val merge_lists : 'a list -> 'b list -> f:('a -> 'b -> 'c) -> 'c list = <fun>
# merge_lists [1;2;3] [-1] ~f:(+);;
Exception: (Assert_failure //toplevel// 6 15).
```

This shows what's special about `assert`, which is that it captures
the line number and character offset of the source location from which
the assertion was made.

### Exception handlers

So far, we've only seen exceptions fully terminate the execution of a
computation.  But often, we want a program to be able to respond to
and recover from an exception.  This is achieved through the use of
_exception handlers_.

In OCaml, an exception handler is declared using a `try`/`with`
statement.  Here's the basic syntax.

```ocaml
try <expr> with
| <pat1> -> <expr1>
| <pat2> -> <expr2>
...
```

A `try/with` clause first evaluates its body, `<expr>`.  If no
exception is thrown, then the result of evaluating the body is what
the entire `try/with` clause evaluates to.

But if the evaluation of the body throws an exception, then the
exception will be fed to the pattern match statements following the
`with`.  If the exception matches a pattern, then we consider the
exception caught, and the `try/with` clause evaluates to the
expression on the right-hand side of the matching pattern.

Otherwise, the original exception continues up the stack of function
calls, to be handled by the next outer exception handler.  If the
exception is never caught, it terminates the program.

### Cleaning up in the presence of exceptions

One headache with exceptions is that they can terminate your execution
at unexpected places, leaving your program in an awkward state.
Consider the following code snippet:

```ocaml
let load_config filename =
  let inc = In_channel.create filename in
  let config = Config.t_of_sexp (Sexp.input_sexp inc) in
  In_channel.close inc;
  config
```

The problem with this code is that the function that loads the
s-expression and parses it into a `Config.t` might throw an exception
if the config file in question is malformed.  Unfortunately, that
means that the `In_channel.t` that was opened will never be closed,
leading to a file-descriptor leak.

We can fix this using Core's `protect` function.  The purpose of
`protect` is to ensure that the `finally` thunk will be called when
`f` exits, whether it exits normally or with an exception.  This is
similar to the `try/finally` construct available in many programming
languages, but it is implemented in a library, rather than being a
built-in primitive.  Here's how it could be used to fix `load_config`.

```ocaml
let load_config filename =
  let inc = In_channel.create filename in
  protect ~f:(fun () -> Config.t_of_sexp (Sexp.input_sexp inc))
    ~finally:(fun () -> In_channel.close inc)
```

This is a common enough problem that `In_channel` has a function
called `with_file` that automates this pattern.

```ocaml
let load_config filename =
  In_channel.with_file filename ~f:(fun inc ->
    Config.t_of_sexp (Sexp.input_sexp inc))
```

`In_channel.with_file` is actually built on top of `protect` so that
it can clean up after itself in the presence of exceptions.


### Catching specific exceptions

OCaml's exception-handling system allows you to tune your
error-recovery logic to the particular error that was thrown.  For
example, `List.find_exn` throws `Not_found` when the element in
question can't be found.  You can take advantage of this in your code,
for example, let's define a function called `lookup_weight`, with the
following signature.

```ocaml
val lookup_weight :
  compute_weight:('data -> float) -> ('key * 'data) list -> 'key -> float
```

`lookup_weight ~compute_weight alist key` should return a
floating-point weight by applying `compute_weight` to the data
associated with `key` by `alist`.  If `key` is not found, then it
should return 0.

We can implement `lookup_weight` as follows.

```ocaml
# let lookup_weight ~compute_weight alist key =
    try
      let data = List.Assoc.find_exn alist key in
      compute_weight data
    with
      Not_found -> 0. ;;
val lookup_weight :
  compute_weight:('a -> float) -> ('b * 'a) list -> 'b -> float =
  <fun>
```

Let's think about the behavior of this code in the presence of
exceptions.  In particular, what happens if `compute_weight` throws an
exception?  Ideally, `lookup_weight` should propagate that exception
on, but if the exception happens to be `Not_found`, then that's not
what will happen:

```ocaml
# lookup_weight ~compute_weight:(fun _ -> raise Not_found)
    ["a",3; "b",4] "a" ;;
- : float = 0.
```

This kind of problem is hard to detect in advance, because the type
system doesn't tell you what exceptions a given function might throw.
For this reason, it's generally better to avoid relying on the
identity of the exception to determine the nature of a failure.  A
better approach is to narrow the scope of the exception handler, so
that when it fires it's very clear what part of the code failed.

```ocaml
# let lookup_weight ~compute_weight alist key =
    match
      try Some (List.Assoc.find_exn alist key)
      with _ -> None
    with
    | None -> 0.
    | Some data -> compute_weight data ;;
val lookup_weight :
  compute_weight:('a -> float) ->
  ('b, 'a) Core.Std.List.Assoc.t -> 'b -> float = <fun>
```

At which point, it makes sense to simply use the non-exception
throwing function, `List.Assoc.find`, instead.

```ocaml
# let lookup_weight ~compute_weight alist key =
    match List.Assoc.find alist key with
    | None -> 0.
    | Some data -> compute_weight data ;;
val lookup_weight :
  compute_weight:('a -> float) ->
  ('b, 'a) Core.Std.List.Assoc.t -> 'b -> float = <fun>
```


### Backtraces

A big part of the point of exceptions is to give useful debugging
information.  But at first glance, OCaml's exceptions can be less than
informative.   Consider the following simple program.

```ocaml
(* exn.ml *)

open Core.Std
exception Empty_list

let list_max = function
  | [] -> raise Empty_list
  | hd :: tl -> List.fold tl ~init:hd ~f:(Int.max)

let () =
  printf "%d\n" (list_max [1;2;3]);
  printf "%d\n" (list_max [])
```

If we build and run this program, we'll get a pretty uninformative
error:

```bash
$ ./exn
3
Fatal error: exception Exn.Empty_list
```

The example in question is short enough that it's quite easy to see
where the error came from.  But in a complex program, simply knowing
which exception was thrown is usually not enough information to figure
out what went wrong.

We can get more information from OCaml if we turn on stack backtraces.
A backtrace is essentially a summary of the stack of calls that were
executed to get to the point where the exception was thrown.
Backtraces can be enabled by setting the `OCAMLRUNPARAM` environment
variable as shown.

```bash
exn $ export OCAMLRUNPARAM=b=1
exn $ ./exn
3
Fatal error: exception Exn.Empty_list
Raised at file "exn.ml", line 7, characters 16-26
Called from file "exn.ml", line 12, characters 17-28
```

You can also capture a backtrace within your program by calling
`Exn.backtrace`, which returns the backtrace of the most recently
thrown exception.  This is useful for reporting detailed information
on errors that did not cause your program to fail.

### From exceptions to error-aware types and back again ###

Both exceptions and error-aware types are necessary parts of
programming in OCaml.  As such, you often need to move between these
two worlds.  Happily, Core comes with some useful helper functions to
help you do just that.  For example, given a piece of code that can
throw an exception, you can capture that exception into an option as
follows:

```ocaml
# let find alist key =
    Option.try_with (fun () -> find_exn alist key) ;;
val find : (string * 'a) list -> string -> 'a option = <fun>
# find ["a",1; "b",2] "c";;
- : int Core.Std.Option.t = None
# find ["a",1; "b",2] "b";;
- : int Core.Std.Option.t = Some 2
```

And `Result` and `Or_error` have similar `try_with` functions.  So, we
could write:

```ocaml
# let find alist key =
    Result.try_with (fun () -> find_exn alist key) ;;
val find : (string * 'a) list -> string -> ('a, exn) Result.t = <fun>
# find ["a",1; "b",2] "c";;
- : (int, exn) Result.t = Result.Error Key_not_found("c")
```

And then we can re-raise that exception:

```ocaml
# Result.ok_exn (find ["a",1; "b",2] "b");;
- : int = 2
# Result.ok_exn (find ["a",1; "b",2] "c");;
Exception: Key_not_found("c").
```


