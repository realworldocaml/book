# A Guided Tour

This chapter gives an overview of OCaml by walking through a series of
small examples that cover most of the major features of the language.
This should give a sense of what OCaml can do, without getting too
deep in any one topic.

We'll present this guided tour using the Core standard library and the
`utop` OCaml toplevel, a shell that lets you type in expressions and
evaluate them interactively.  `utop` is an easier-to-use version of
the standard toplevel (which you can start by typing `ocaml` at the
command line).  These instructions will assume you're using `utop`
specifically.

Before getting started, do make sure you have a working OCaml
installation and toplevel as you read through this chapter so you can
try out the examples.

<note>
<title>Installing `utop`</title>

The easiest way to get the examples running is to set up the OPAM
package manager, which is explained in [xref](#installation).  In a
nutshell, you need to have a working C compilation environment and the
PCRE library installed, and then:

```
$ opam init
$ opam switch 4.01.0dev+trunk
$ opam install utop core_extended
$ eval `opam config -env`
```

Note that the above commands will take some time to run.  When they're
done, create a file called `~/.ocamlinit` in your home directory:

```ocaml
#use "topfind"
#camlp4o
#thread
#require "core.top"
```

Then type in `utop`, and you'll be in an interactive toplevel environment.
OCaml phrases are only evaluated when you enter a double semicolon (`;;`), so
you can split your typing over multiple lines.  You can exit `utop` by pressing
`control-D` and return. For complete instructions, please refer to
[xref](#installation).

</note>

## OCaml as a calculator

Let's spin up `utop`.  Throughout the book we're going to use Core, a
more full-featured and capable replacement for OCaml's standard
library.  Accordingly, we'll start by opening the `Core.Std` module to
get access to Core's libraries.  If you don't open `Core.Std` many of
the examples below will fail.

```ocaml
$ utop
# open Core.Std;;
```

Now that we have Core open, let's try a few simple numerical
calculations.

```ocaml
# 3 + 4;;
- : int = 7
# 8 / 3;;
- : int = 2
# 3.5 +. 6.;;
- : float = 9.5
# 30_000_000 / 300_000;;
- : int = 100
# sqrt 9.;;
- : float = 3.
```

By and large, this is pretty similar to what you'd find in any
programming language, but there are a few things that jump right out
at you.

- We needed to type `;;` in order to tell the toplevel that it should
  evaluate an expression.  This is a peculiarity of the toplevel that
  is not required in stand-alone programs (though it is sometimes
  helpful to include `;;` to improve OCaml's error reporting.)
- After evaluating an expression, the toplevel spits out both the type
  of the result and the result itself.
- Function arguments are separated by spaces, instead of by
  parenthesis and commas, which is more like the UNIX shell than C or
  Java.
- OCaml allows you to place underscores in the middle of your integer
  literals, as a way of improving readability.  Note that underscores
  can be placed anywhere in within the number, not just every three
  digits.
- OCaml carefully distinguishes between `float`, the type for floating
  point numbers and `int` the type for integers.  The types have
  different literals (`6.` instead of `6`) and different infix
  operators (`+.` instead of `+`), and OCaml doesn't automatically
  cast between types.  This can be a bit of a nuisance, but it has its
  benefits, since it prevents some kinds of bugs that arise in other
  languages due to unexpected differences between the behavior of
  `int` and `float`.

We can also create a variable to name the value of a given expression,
using the `let` keyword (also known as a _let binding_).

```ocaml
# let x = 3 + 4;;
val x : int = 7
# let y = x + x;;
val y : int = 14
```

After a new variable is created, the toplevel tells us the name of the
variable (`x` or `y`), in addition to its type (`int`) and value (`7` or `14`).

## Functions and type Inference

The `let` syntax can also be used for creating functions.

```ocaml
# let square x = x * x ;;
val square : int -> int = <fun>
# square 2;;
- : int = 4
# square (square 2);;
- : int = 16
```

Functions in OCaml are values like any other, which is why we bind one
to a variable using the same `let` keyword used for binding a variable
to a simple value such as an integer.

When using `let` to define a function, the first identifier after the
`let` is the function name, and each subsequent identifier is a
different argument to the function.  Thus, `square` is a function with
a single argument.  If no arguments are given, then we just have the
ordinary definition of a variable that we saw earlier.

Now that we're creating more interesting values like functions, the
types have gotten more interesting too.  `int -> int` is a function
type, in this case indicating a function that takes an `int` and
returns an `int`.  We can also write functions that take multiple
arguments.

```ocaml
# let ratio x y =
     Float.of_int x /. Float.of_int y
  ;;
val ratio : int -> int -> float = <fun>
# ratio 4 7;;
- : float = 0.571428571428571397
```

As a side note, the above is our first use of OCaml modules.  Here,
`FLoat.of_int` refers to the `of_int` function contained in the
`FLoat` module, and not, as you might expect from an object-oriented
language, accessing a method of an object.  The `Float` module in
particular contains `of_int` as well as many other useful functions
for dealing with floats.

The notation for the type-signature of a multi-argument functions may
be a little surprising at first, but we'll explain where it comes from
when we get to function currying in [xref](#multi-argument-functions).
For the moment, think of the arrows as separating different arguments
of the function, with the type after the final arrow being the return
value of the function.  Thus, `int -> int -> float` describes a
function that takes two `int` arguments and returns a `float`.

We can even write functions that take other functions as arguments.
Here's an example of a function that takes three arguments: a test
function and two integer arguments.  The function returns the sum of
the integers that pass the test.

```ocaml
# let sum_if_true test first second =
    (if test first then first else 0)
    + (if test second then second else 0)
  ;;
val sum_if_true : (int -> bool) -> int -> int -> int = <fun>
```

If we look at the inferred type signature in detail, we see that the
first argument is a function that takes an integer and returns a
boolean, and that the remaining two arguments are integers.  Here's an
example of this function in action.

```ocaml
# let even x =
    x mod 2 = 0 ;;
val even : int -> bool = <fun>
# sum_if_true even 3 4;;
- : int = 4
# sum_if_true even 2 4;;
- : int = 6
```

Note that in the definition of `even` we used `=` in two different
ways: once as the part of the let binding that separates the thing
being defined from its definition; and once as an equality test, when
comparing `x mod 2` to `0`.  These two uses of `=` are basically
unrelated.

### Type inference

As the types we encounter get more complicated, you might ask yourself
how OCaml is able to figure them out, given that we didn't write down
any explicit type information.

OCaml determines the type of an expression using a technique called
_type inference_, by which it infers the type of a given expression
based on what it already knows about the types of other related
variables, and on constraints on the types that arise from the
structure of the code.

As an example, let's walk through the process of inferring the type of
`sum_if_true`.

- OCaml requires that both arms of an `if` statement return the same
  type, so the expression `if test x then x else 0` requires that `x`
  must be the same type as `0`, which is `int`.  By the same logic we
  can conclude that `y` has type `int`.
- `test` is passed `x` as an argument.  Since `x` has type `int`, the
  input type of `test` must be `int`.
- `test x` is used as the condition in an `if` statement, so the
  return type of `test` must be `bool`.
- The fact that `+` returns an int implies that the return value of
  `sum_if_true` must be int.

Together, that nails down the types of all the variables, which
determines the overall type of `sum_if_true`.

Over time, you'll build a rough intuition for how the OCaml inference
engine works, which makes it easier to reason through your programs.
One way of making it easier to understand the types is to add explicit
type annotations.  These annotations never change the behavior of an
OCaml program, but they can serve as useful documentation, as well as
catch unintended type changes.  Here's an annotated version of
`sum_if_true`:

```ocaml
# let sum_if_true (test : int -> bool) (x : int) (y : int) : int =
     (if test x then x else 0)
     + (if test y then y else 0)
  ;;
val sum_if_true : (int -> bool) -> int -> int -> int = <fun>
```

In the above, we've marked every argument to the function with its
type, with the final annotation indicating the type of the return
value.  Such type annotations can actually go around any value in an
OCaml program, and can be useful for figuring out why a given program
is failing to compile.

### Inferring generic types

Sometimes, there isn't enough information to fully determine the
concrete type of a given value.  Consider this function:

```ocaml
# let first_if_true test x y =
    if test x then x else y
  ;;
```

`first_if_true` takes as its arguments a function `test`, and two
values, `x` and `y`, where `x` is to be returned if `test x` evaluates
to `true`, and `y` otherwise.  So what's the type of `first_if_true`?
There are no obvious clues such as arithmetic operators or literals to
tell you what the type of `x` and `y` are.  That makes it seem like
one could use this `first_if_true` on values of any type.  Indeed, if
we look at the type returned by the toplevel:

```ocaml
val first_if_true : ('a -> bool) -> 'a -> 'a -> 'a = <fun>
```

we see that rather than choose a single concrete type, OCaml has
introduced a _type variable_ `'a` to express that the type is generic.
In particular, the type of the `test` argument is `('a -> bool)`,
which means that test is a one-argument function whose return value is
`bool`, and whose argument could be of any type `'a`.  But, whatever
type `'a` is, it has to be the same as the type of the other two
arguments, `x` and `y`, and of the return value of `first_if_true`.
This kind of genericity is called _parametric polymorphism_, and is
very similar to generics in C# and Java.

The generic type of `first_if_true` allows us to write:

```ocaml
# let long_string s = String.length s > 6;;
val long_string : string -> bool = <fun>
# first_if_true long_string "short" "loooooong";;
- : string = "loooooong"
```

as well as:

```ocaml
# let big_number x = x > 3;;
val big_number : int -> bool = <fun>
# first_if_true big_number 4 3;;
- : int = 4
```

Both `long_string` and `big_number` are functions, and each is passed
to `first_if_true` with two other arguments of the appropriate type
(strings in the first example, and integers in the second).  But we
can't mix and match two different concrete types for `'a` in the same
use of `first_if_true`.

```ocaml
# first_if_true big_number "short" "loooooong";;
Characters 25-30:
  first_if_true big_number "short" "loooooong";;
                           ^^^^^^^
Error: This expression has type string but
    an expression was expected of type int
```

In this example, `big_number` requires that `'a` be instantiated as
`int`, whereas `"short"` and `"loooooong"` require that `'a` be
instantiated as `string`, and they can't both be right at the same
time.

<note><title>Type errors vs exceptions</title>

There's a big difference in OCaml (and really in any compiled
language) between errors that are caught at compile time and those
that are caught at run-time.  It's better to catch errors as early as
possible in the development process, and compilation time is best of
all.

Working in the toplevel somewhat obscures the difference between
run-time and compile time errors, but that difference is still there.
Generally, type errors, like this one:

```ocaml
# let add_potato x =
     x + "potato";;
  Characters 28-36:
       x + "potato";;
           ^^^^^^^^
Error: This expression has type string but an expression was expected of type
         int
```

are compile-time errors, whereas an error that can't be caught by the
type system, like division by zero, leads to a runtime exception.

```ocaml
# let is_a_multiple x y =
     x mod y = 0 ;;
  val is_a_multiple : int -> int -> bool = <fun>
# is_a_multiple 8 2;;
- : bool = true
# is_a_multiple 8 0;;
Exception: Division_by_zero.
```

The distinction here is that type errors will stop you whether or not
the offending code is ever actually executed.  Merely defining
`add_potato` is an error, whereas `is_a_multiple` only fails when it's
called, and then, only when it's called with an input that triggers
the exception.

</note>

## Tuples, Lists, Options and Pattern-matching

### Tuples

So far we've encountered a handful of basic types like `int`, `float`
and `string` as well as function types like `string -> int`.  But we
haven't yet talked about any data structures.  We'll start by looking
at a particularly simple data structure, the tuple.  A tuple is an
ordered collection of values that can each be of different type.  You
can create a tuple by joining values together with a comma:

```ocaml
# let a_tuple = (3,"three");;
val a_tuple : int * string = (3, "three")
```

For the mathematically inclined, the `*` character is used because the
set of all pairs of type `t * s` corresponds to the Cartesian product
of the set of elements of type `t` and the set of elements of type
`s`.

You can extract the components of a tuple using OCaml's
pattern-matching syntax. For example:

```ocaml
# let (x,y) = a_tuple;;
val x : int = 3
val y : string = "three"
```

Here, the `(x,y)` on the left-hand side of the `let` binding is the
pattern.  This pattern lets us mint the new variables `x` and `y`,
each bound to different components of the value being matched, which
can now be used in subsequent expressions.

```ocaml
# x + String.length y;;
- : int = 8
```

Note that the same syntax is used both for constructing and for
pattern-matching on tuples.

Pattern matching can also show up in function arguments.  Here's a
function for computing the distance between two points on the plane,
where each point is represented as a pair of `float`s.  The pattern
matching syntax lets us get at the values we need with a minimum of
fuss.

```ocaml
# let distance (x1,y1) (x2,y2) =
    sqrt ((x1 -. x2) ** 2. +. (y1 -. y2) ** 2.)
  ;;
val distance : float * float -> float * float -> float = <fun>
```

The `**` operator used above is for raising a floating-point number to
a power.

This is just a first taste of pattern matching.  Pattern matching is a
pervasive tool in OCaml, and as you'll see, it has surprising power.

### Lists

Where tuples let you combine a fixed number of items, potentially of
different types, lists let you hold any number of items of the same
type.  For example:

```ocaml
# let languages = ["OCaml";"Perl";"C"];;
val languages : string list = ["OCaml"; "Perl"; "C"]
```

Note that you can't mix elements of different types on the same list,
as we did with tuples.

```ocaml
# let numbers = [3;"four";5];;
Characters 17-23:
  let numbers = [3;"four";5];;
                   ^^^^^^
Error: This expression has type string but an expression was expected of type
         int
```

#### The `List` module

Core comes with a `List` module that has a rich collection of
functions for working with lists.  We can access values from within a
module by using dot-notation.  Here, for example, is how we compute
the length of a list.

```ocaml
# List.length languages;;
- : int = 3
```

Here's something a little more complicated.  We can compute the list
of the lengths of each language as follows.

```ocaml
# List.map languages ~f:String.length;;
- : int list = [5; 4; 1]
```

`List.map` takes two arguments: a list and a function for transforming
the elements of that list.  Note that `List.map` creates a new list
and does not modify the original.

In this example, the function `String.length` is passed under the
_labeled argument_ `~f`.  Labels allow you to specify function
arguments by name rather than by position.  As you can see below, we
can change the order of labeled arguments without changing the
function's behavior.

```ocaml
# List.map ~f:String.length languages;;
- : int list = [5; 4; 1]
```

We'll learn more about labeled arguments and why they're important in
[xref](#variables-and-functions).


#### Constructing lists with `::`

In addition to constructing lists using brackets, we can use the
operator `::` for adding elements to the front of a list.

```ocaml
# "French" :: "Spanish" :: languages;;
- : string list = ["French"; "Spanish"; "OCaml"; "Perl"; "C"]
```

Here, we're creating a new and extended list, not changing the list we
started with, as you can see below.

```ocaml
# languages;;
- : string list = ["OCaml"; "Perl"; "C"]
```

The bracket notation for lists is really just syntactic sugar for
`::`.  Thus, the following declarations are all equivalent.  Note that
`[]` is used to represent the empty list.

```ocaml
# [1; 2; 3];;
- : int list = [1; 2; 3]
# 1 :: (2 :: (3 :: []));;
- : int list = [1; 2; 3]
# 1 :: 2 :: 3 :: [];;
- : int list = [1; 2; 3]
```

The `::` operator can only be used for adding one element to the front
of the list, with the list terminating at `[]`, the empty list.
There's also a list concatenation operator, `@`, which can concatenate
two lists.

```ocaml
# [1;2;3] @ [4;5;6];;
- : int list = [1; 2; 3; 4; 5; 6]
```

It's important to remember that this is not a constant-time operation.
Concatenating two lists takes time proportional to the length of the
first list.

#### List patterns using `match`

The elements of a list can be accessed through pattern-matching.  List
patterns are based on the two list constructors, `[]` and `::`.
Here's a simple example.

```ocaml
# let my_favorite_language (my_favorite :: the_rest) =
     my_favorite
  ;;
```

By pattern matching using `::`, we've isolated and named the first
element of the list (`my_favorite`) and the remainder of the list
(`the_rest`).  If you know Lisp or Scheme, what we've done is the
equivalent of using the functions `car` and `cdr` to isolate the first
element of a list and the remainder of that list.

If you try the above example in the toplevel, however, you'll see that
it spits out a warning:

```ocaml
    Characters 25-69:
  .........................(my_favorite :: the_rest) =
       my_favorite
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
[]
val my_favorite_language : 'a list -> 'a = <fun>
```

The warning comes because the compiler can't be certain that the
pattern match won't lead to a runtime error.  Indeed, the warning
gives an example of a list, (`[]`, the empty list) that doesn't match
the provided pattern.  Indeed, if we try to run
`my_favorite_language`, we'll see that it works on non-empty list, and
fails on empty ones.

```ocaml
# my_favorite_language ["English";"Spanish";"French"];;
- : string = "English"
# my_favorite_language [];;
Exception: Match_failure ("//toplevel//", 11, 10).
```

You can avoid these warnings, and more importantly make sure that your
code actually handles all of the possible cases, by using a `match`
statement instead.

A `match` statement is a kind of juiced-up version of the switch
statement found in `C` and `Java`.  It essentially lets you list a
sequence of patterns (separated by `|` characters --- the one before
the first case is optional), and the compiler then dispatches to the
code following the first matched pattern.  And, as we've already seen,
we can name new variables in our patterns that correspond to
sub-structures of the value being matched.

Here's a new version of `my_favorite_language` that uses `match` and
doesn't trigger a compiler warning.

```ocaml
# let my_favorite_language languages =
    match languages with
    | first :: the_rest -> first
    | [] -> "OCaml" (* A good default! *)
 ;;
val my_favorite_language : string list -> string = <fun>
# my_favorite_language ["English";"Spanish";"French"];;
- : string = "English"
# my_favorite_language [];;
- : string = "OCaml"
```

Note that we included a comment in the above code.  OCaml comments are
bounded by `(*` and `*)`, and can be nested arbitrarily and cover
multiple lines.  There's no equivalent of C-style single line
comments that are prefixed by `//`.

The first pattern, `first :: the_rest`, covers the case where
`languages` has at least one element, since every list except for the
empty list can be written down with one or more `::`'s.  The second
pattern, `[]`, matches only the empty list.  These cases are
exhaustive (every list is either empty, or has at least one element),
and the compiler can detect that exhaustiveness, which is why it
doesn't spit out a warning.


#### Recursive list functions

Recursive functions, or, functions that call themselves, are an
important technique in OCaml and in any functional language.  The
typical approach to designing a recursive function is to separate the
logic into a set of _base cases_, that can be solved directly, and a
set of _inductive cases_, where the function breaks the problem down
into smaller pieces and then calls itself to solve those smaller
problems.

When writing recursive list functions, this separation between the
base cases and the inductive cases is often done using pattern
matching.  Here's a simple example of a function that sums the
elements of a list.

```ocaml
# let rec sum l =
    match l with
    | [] -> 0                   (* base case *)
    | hd :: tl -> hd + sum tl   (* inductive case *)
  ;;
val sum : int list -> int
# sum [1;2;3];;
- : int = 6
```

Following the common OCaml idiom, we use `hd` to refer to the head of
the list and `tl` to refer to the tail.  Note that we had to use the
`rec` keyword to allow `sum` to refer to itself.  As you might
imagine, the base case and inductive case are different arms of the
match.

Logically, you can think of the evaluation of a simple recursive
function like `sum` almost as if it were a mathematical equation whose
meaning you were unfolding step by step.

```ocaml
sum [1;2;3]
1 + sum [2;3]
1 + (2 + sum [3])
1 + (2 + (3 + sum []))
1 + (2 + (3 + 0))
1 + (2 + 3)
1 + 5
6
```

This suggests a reasonable mental model for what OCaml is actually
doing to evaluate a recursive function.

We can introduce more complicated list patterns as well.  Here's a
function for destuttering a list, _i.e._, for removing sequential
duplicates.

```ocaml
# let rec destutter list =
    match list with
    | [] -> []
    | hd1 :: hd2 :: tl ->
      if hd1 = hd2 then destutter (hd2 :: tl)
      else hd1 :: destutter (hd2 :: tl)
  ;;
```

Again, the first arm of the match is the base case, and the second is
the inductive.  Unfortunately, this code has a problem.  If you type
it into the toplevel, you'll see this error:

```
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
_::[]
```

This indicates that we're missing a case, in particular we don't
handle one-element lists.  That's easy enough to fix by adding another
case to the match:

```ocaml
# let rec destutter list =
    match list with
    | [] -> []
    | hd :: [] -> hd :: []
    | hd1 :: hd2 :: tl ->
      if hd1 = hd2 then destutter (hd2 :: tl)
      else hd1 :: destutter (hd2 :: tl)
  ;;
val destutter : 'a list -> 'a list = <fun>
# destutter ["hey";"hey";"hey";"man!"];;
- : string list = ["hey"; "man!"]
```

Note that this code used another variant of the list pattern, `[hd]`,
to match a list with a single element.  We can do this to match a list
with any fixed number of elements, _e.g._, `[x;y;z]` will match any
list with exactly three elements, and will bind those elements to the
variables `x`, `y` and `z`.

In the last few examples, our list processing code involved a lot of
recursive functions.  In practice, this isn't usually necessary.  Most
of the time, you'll find yourself happy to use the iteration functions
found in the `List` module.  But it's good to know how to use
recursion when you need to do something new that's not already
supported.

### Options

Another common data structure in OCaml is the option.  An option is
used to express that a value might or might not be present.  For
example,

```ocaml
# let divide x y =
    if y = 0 then None else Some (x/y) ;;
val divide : int -> int -> int option = <fun>
```

The function `divide` either returns `None`, if the divisor is zero,
or `Some` of the result of the division, otherwise.  `Some` and `None`
are constructors, like `::` and `[]` for lists, which let you build
optional values.  You can think of an option as a specialized list
that can only have zero or one element.

To examine the contents of an option, we use pattern matching, as we
did with tuples and lists.  Consider the following simple function for
printing a log entry given an optional time and a message.  If no time
is provided (_i.e._, if the time is `None`), the current time is
computed and used in its place.

```ocaml
# let print_log_entry maybe_time message =
    let time =
      match maybe_time with
      | Some x -> x
      | None -> Time.now ()
    in
    printf "%s: %s\n" (Time.to_sec_string time) message ;;
val print_log_entry : Time.t option -> string -> unit
# print_log_entry (Some Time.epoch) "A long long time ago";;
1969-12-31 19:00:00: A long long time ago
- : unit = ()
# print_log_entry None "Up to the minute";;
2013-02-23 16:49:25: Up to the minute
- : unit = ()
```

We use a `match` statement for handling the two possible states of an
option.

<note> <title> Nesting `let`s with `let` and `in` </title>

As a side note, this is our first use of `let` to define a new
variable within the body of a function.  A `let` bounded with an `in`
can be used to introduce a new binding within any local scope,
including a function body.  The `in` marks the beginning of the scope
within which the new variable can be used.  Thus, we could write:

```ocaml
# let x = 7 in
  x + x
  ;;
- : int = 14
```

Note that the scope of the let binding is terminated by the
double-semicolon.

We can also have multiple let statements in a row, each one adding a
new variable binding to what came before.

```ocaml
# let x = 7 in
  let y = x * x in
  x + y
  ;;
- : int = 56
```

This kind of nested let binding is a common way of building up a
complex expression, with each `let` breaking off and naming an
individual component, and then combining them in one final expression.

</note>

Options are important because they are the standard way in OCaml to
encode a value that might not be there --- there's no such thing as a
`NullPointerException` in OCaml.  This is different from most other
languages, including Java and C#, where most if not all datatypes are
_nullable_, meaning that, whatever their type is, any given value also
contains the possibility of being a null value.  In such languages,
null is lurking everywhere.

In OCaml, however, nulls are explicit.  A value of type `string *
string` always actually contains two well-defined values of type
`string`.  If you want to allow, say, the first of those to be absent,
then you need to change the type to `string option * string`.  As
we'll see, this explicitness allows the compiler to provide a great
deal of help in making sure you're correctly handing the possibility
of missing data.

## Records and Variants

So far, we've looked only at data structures that were predefined in
the language, like lists and tuples.  But OCaml also allows us to
define new datatypes.  Here's a toy example of a datatype representing
a point in 2-dimensional space:

```ocaml
# type point2d = { x : float; y : float };;
type point2d = { x : float; y : float; }
```

`point2d` is a _record_ type, which you can think of as a tuple where
the individual fields are named, rather than being defined
positionally.  Record types are easy enough to construct:

```ocaml
# let p = { x = 3.; y = -4. };;
val p : point2d = {x = 3.; y = -4.}
```

And we can get access to the contents of these types using pattern
matching:

```ocaml
# let magnitude { x = x_pos; y = y_pos } =
    sqrt (x_pos ** 2. +. y_pos ** 2.);;
val magnitude : point2d -> float = <fun>
```

We can write the pattern match even more tersely, using what's called
_field punning_.  In particular, when the name of the field and the
name of the variable coincide, we don't have to write them both down.
Thus, the magnitude function can be rewritten as follows.

```ocaml
# let magnitude { x; y } = sqrt (x ** 2. +. y ** 2.);;
```

We can also use dot-notation for accessing record fields:

```ocaml
# let distance v1 v2 =
     magnitude { x = v1.x -. v2.x; y = v1.y -. v2.y };;
val distance : point2d -> point2d -> float = <fun>
```

And we can of course include our newly defined types as components in
larger types, as in the following types, each of which is a
description of a different geometric object.

```ocaml
# type circle_desc  = { center: point2d; radius: float }
  type rect_desc    = { lower_left: point2d; width: float; height: float }
  type segment_desc = { endpoint1: point2d; endpoint2: point2d } ;;
```

Now, imagine that you want to combine multiple objects of these types
together as a description of a multi-object scene.  You need some
unified way of representing these objects together in a single type.
One way of doing this is using a _variant_ type:

```ocaml
# type scene_element =
    | Circle  of circle_desc
    | Rect    of rect_desc
    | Segment of segment_desc
  ;;
```

The `|` character separates the different cases of the variant (the
first `|` is optional), and each case has a tag, like `Circle`, `Rect`
and `Segment`, to distinguish that case from the others.  Here's how we
might write a function for testing whether a point is in the interior
of some element of a list of `scene_element`s.

```ocaml
# let is_inside_scene_element point scene_element =
     match scene_element with
     | Circle { center; radius } ->
       distance center point < radius
     | Rect { lower_left; width; height } ->
       point.x > lower_left.x && point.x < lower_left.x +. width
       && point.y > lower_left.y && point.y < lower_left.y +. height
     | Segment { endpoint1; endpoint2 } -> false
  ;;
val is_inside_scene_element : point2d -> scene_element -> bool = <fun>
# let is_inside_scene point scene =
     List.exists scene
       ~f:(fun el -> is_inside_scene_element point el)
   ;;
val is_inside_scene : point2d -> scene_element list -> bool = <fun>
# is_inside_scene {x=3.;y=7.}
    [ Circle {center = {x=4.;y= 4.}; radius = 0.5 } ];;
- : bool = false
# is_inside_scene {x=3.;y=7.}
    [ Circle {center = {x=4.;y= 4.}; radius = 5.0 } ];;
- : bool = true
```

You might at this point notice that the use of `match` here is
reminiscent of how we used `match` with `option` and `list`.  This is
no accident: `option` and `list` are really just examples of variant
types that happen to be important enough to be defined in the standard
library (and in the case of lists, to have some special syntax).

We also made our first use of an _anonymous function_ in the call to
`List.exists`.  An anonymous function is a function that is defined
but not named, in this case, using the `fun` keyword.  Anonymous
functions are common in OCaml, particularly when using iteration
functions like `List.exists`.

The purpose of `List.exists` is to check if there are any elements of
the given list in question on which the provided function evaluates to
`true`.  In this case, we're using `List.exists` to check if there is
a scene element within which our point resides.

## Imperative programming

So far, we've only written so-called _pure_ or _functional_ code,
meaning that we didn't write any code that modified a variable or
value after its creation.  Indeed, almost all of the data structures
we've encountered so far are _immutable_, meaning there's no way in
the language to modify them at all.  This is a quite different style
from _imperative_ programming, where computations are structured as
sequences of instructions that operate by modifying state as they go.

Functional code is the default in OCaml, with variable bindings and
most data structures being immutable.  But OCaml also has excellent
support for imperative programming, including mutable data structures
like arrays and hashtables and control-flow constructs like for and
while loops.

### Arrays

Perhaps the simplest mutable data structure in OCaml is the array.
Arrays in OCaml are very similar to arrays in other languages like C:
indexing starts at 0, and accessing or modifying an array element is a
constant-time operation.  Arrays are more compact in terms of memory
utilization than most other data structures in OCaml, including lists.
Here's an example:

```ocaml
# let numbers = [| 1;2;3;4 |];;
val numbers : int array = [|1; 2; 3; 4|]
# numbers.(2) <- 4;;
- : unit = ()
# numbers;;
- : int array = [|1; 2; 4; 4|]
```

the `.(i)` syntax is used to refer to an element of an array, and the
`<-` syntax is for modification. Because the elements of the array are
counted starting at zero, element `.(2)` is the third element.

### Mutable record fields

The array is an important mutable data structure, but it's not the only
one.  Records, which are immutable by default, can be declared with
specific fields as being mutable.  Here's a small example of a
data structure for storing a running statistical summary of a
collection of numbers.  Here's the basic data structure:

```ocaml
# type running_sum =
   { mutable sum: float;
     mutable sum_sq: float; (* sum of squares *)
     mutable samples: int;
   }
  ;;
```

The fields in `running_sum` are designed to be easy to extend
incrementally, and sufficient to compute means and standard
deviations, as shown below.

```ocaml
# let mean rsum = rsum.sum /. float rsum.samples
  let stdev rsum =
     sqrt (rsum.sum_sq /. float rsum.samples
           -. (rsum.sum /. float rsum.samples) ** 2.) ;;
val mean : running_sum -> float = <fun>
val stdev : running_sum -> float = <fun>
```

We also need functions to create and update `running_sum`s:

```ocaml
# let create () = { sum = 0.; sum_sq = 0.; samples = 0 }
  let update rsum x =
     rsum.samples <- rsum.samples + 1;
     rsum.sum     <- rsum.sum     +. x;
     rsum.sum_sq  <- rsum.sum_sq  +. x *. x
  ;;
val create : unit -> running_sum = <fun>
val update : running_sum -> float -> unit = <fun>
```

`create` returns a `running_sum` corresponding to the empty set, and
`update rsum x` changes `rsum` to reflect the addition of `x` to its
set of samples, by updating the number of samples, the sum, and the
sum of squares.

Note the use in the above code of single semi-colons to sequence
operations.  When we were working purely functionally, this wasn't
necessary, but you start needing it when your code is acting by
side-effect.

A new and somewhat odd type has cropped up in this example: `unit`.
What makes `unit` different is that there is only one value of type
`unit`, which is written `()`.  Because there is only one value of
type `unit` that value doesn't really convey any information.

If it doesn't convey any information, then what is `unit` good for?
Most of the time, `unit` acts as a placeholder.  Thus, we use `unit`
for the return value of a function like `update` that operates by side
effect rather than by returning a value, and for the argument to a
function like `create` that doesn't require any information to be
passed into it in order to run.  This is similar to the role that
`void` plays in languages like C and Java.

Here's an example of `create` and `update` in action.

```ocaml
# let rsum = create ();;
val rsum : running_sum = {sum = 0.; sum_sq = 0.; samples = 0}
# List.iter [1.;3.;2.;-7.;4.;5.] ~f:(fun x -> update rsum x);;
- : unit = ()
# mean rsum;;
- : float = 1.33333333333333326
# stdev rsum;;
- : float = 3.94405318873307698
```

### Refs

We can declare a single mutable value by using a `ref`, which is a
record type with a single mutable field that is defined in the
standard library.

```ocaml
# let x = { contents = 0 };;
val x : int ref = {contents = 0}
# x.contents <- x.contents + 1;;
- : unit = ()
# x;;
- : int ref = {contents = 1}
```

There are a handful of useful functions and operators defined for refs
to make them more convenient to work with.

```ocaml
# let x = ref 0 ;; (* create a ref, i.e., { contents = 0 } *)
val x : int ref = {contents = 0}
# !x ;;            (* get the contents of a ref, i.e., x.contents *)
- : int = 0
# x := !x + 1 ;;   (* assignment, i.e., x.contents <- ... *)
- : unit = ()
# !x ;;
- : int = 1
```

The definition of all this is quite straightforward.  Here is the
complete implementation of the `ref` type.  The `'a` before the ref
indicates that the `ref` type is polymorphic, in the same way that
lists are polymorphic, meaning it can contain values of any type.

```ocaml
type 'a ref = { mutable contents : 'a }

let ref x = { contents = x }
let (!) r = r.contents
let (:=) r x = r.contents <- x
```

Here, `!` and `:=` are infix operators that we're defining, where the
parenthetical syntax marks them as such.

Even though a `ref` is just another record type, it's notable because
it is the standard way of simulating the traditional mutable variable
you'll find in most imperative languages.  For example, we can sum
over the elements of a list imperatively by calling `List.iter` to
call a simple function on every element of a list, using a ref to
accumulate the results.

```ocaml
# let sum list =
    let sum = ref 0 in
    List.iter list ~f:(fun x -> sum := !sum + x);
    !sum
```

This isn't the most idiomatic (or the fastest) way to sum up a list,
but it shows how you can use a ref in place of a mutable variable.

### For and while loops

Along with mutable data structures, OCaml gives you constructs like
while and for loops for interacting with them.  Here, for example, is
some code that uses a for loop for permuting an array.  We use the
`Random` module as our source of randomness.  `Random` starts out with
a deterministic seed, but you can call `Random.self_init` to choose a
new seed at random.

```ocaml
# let permute ar =
    for i = 0 to Array.length ar - 2 do
       (* pick a j that is after i and before the end of the list *)
       let j = i + 1 + Random.int (Array.length ar - i - 1) in
       (* Swap i and j *)
       let tmp = ar.(i) in
       ar.(i) <- ar.(j);
       ar.(j) <- tmp
    done
  ;;
val permute : 'a array -> unit = <fun>
```

From a syntactic perspective, you should note the keywords that
distinguish a for loop: `for`, `to`, `do` and `done`.

Here's an example run of this code.

```ocaml
# let ar = Array.init 20 ~f:(fun i -> i);;
val ar : int array =
  [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19|]
# permute ar;;
- : unit = ()
# ar;;
- : int array =
[|14; 13; 1; 3; 2; 19; 17; 18; 9; 16; 15; 7; 12; 11; 4; 10; 0; 5; 6; 8|]
```

OCaml also supports while loops, as shown in the following function
for finding the first non-negative position in an array.  Note that
`while` (like `for`) is also a keyword.

```ocaml
# let find_first_negative_entry ar =
     let pos = ref 0 in
     while !pos < Array.length ar && ar.(!pos) >= 0 do
       pos := !pos + 1
     done;
     if !pos = Array.length ar then None else Some !pos
  ;;
            val find_first_negative_entry : int Core.Std.Array.t -> int option = <fun>
# find_first_negative_entry [|1;2;0;3|];;
- : int option = None
# find_first_negative_entry [|1;-2;0;3|];;
- : int option = Some 1
```

## A complete program

So far, we've played with the basic features of the language using the
toplevel.  Now we'll create a simple, complete stand-along program
that does something useful: sum up a list of numbers read in from the
standard input.

Here's the code, which you can save in a file called `sum.ml`.

```ocaml
(* file: sum.ml *)

open Core.Std

let rec read_and_accumulate accum =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> accum
  | Some x -> read_and_accumulate (accum +. Float.of_string x)

let () =
  printf "Total: %F\n" (read_and_accumulate 0.)
```

This is our first use of OCaml's input and output routines.  The
function `read_and_accumulate` is a recursive function that uses
`In_channel.input_line` to read in lines one by one from the standard
input, invoking itself at each iteration with its updated accumulated
sum.  Note that `input_line` returns an optional value, with `None`
indicating the end of the input.

After `read_and_accumulate` returns, the total needs to be printed.
This is done using the `printf` command, which provides support for
type-safe format strings, similar to what you'll find in a variety of
languages.  The format string is parsed by the compiler and used to
determine the number and type of the remaining arguments that are
required.  In this case, there is a single formatting directive, `%F`,
so `printf` expects one additional argument of type `float`.

### Compiling and running

We can use `ocamlbuild` to compile the program.  We'll need to create
a file, in the same directory as `sum.ml`, called `_tags`.  We can put
the following in `_tags` to indicate that we're building against Core,
and that threads should be enabled, which is required by Core.

```
true:package(core),thread
```

With our `_tags` file in place, we can build our executable by issuing
this command.

```
ocamlbuild -use-ocamlfind sum.native
```

The `.native` suffix indicates that we're building a native-code
executable, which we'll discuss more in
[xref](#files-modules-and-programs).  Once the build completes, we can
use the resulting program like any command-line utility.  In this
example, we can just type in a sequence of numbers, one per line,
hitting control-d to exit when the input is complete.

```
max $ ./sum.native
1
2
3
94.5
Total: 100.5
```

More work is needed to make a really usable command-line program,
including a proper command-line parsing interface and better error
handling, all of which is covered in [xref](#command-line-parsing).

## Where to go from here

That's it for our guided tour!  There are plenty of features left to
touch upon and lots of details to explain, but the hope is that this
has given you enough of a feel for the language that you have a sense
as to what to expect, and will be comfortable reading examples in the
rest of the book.


