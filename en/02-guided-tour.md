# A Guided Tour

This chapter is going to give an overview of OCaml by walking through
a series of small examples that cover most of the major features.
This should give a sense of what OCaml can do, without going into too
much detail about any particular topic.

We'll present this guided tour using the OCaml toplevel, an
interactive shell that lets you type in expressions and evaluate them
interactively.  When you get to the point of running real programs,
you'll want to leave the toplevel behind, but it's a great tool for
getting to know the language.

You should have a working toplevel as you go through this chapter, so
you can try out the examples as you go.  There is a zero-configuration
browser-based toplevel that you can use for this, which you can find here:

     http://realworldocaml.org/core-top

Or you can install OCaml and Core on your computer directly.
Instructions for this are found in Appendix {???}.

## OCaml as a calculator

Let's spin up the toplevel and open the `Core.Std` module, which gives
us access to Core's libraries, and then try out a few simple numerical
calculations.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
$ rlwrap ocaml
        Objective Caml version 3.12.1

# open Core.Std;;
# 3 + 4;;
- : int = 7
# 8 / 3;;
- : int = 2
# 3.5 +. 6.;;
- : float = 9.5
# sqrt 9.;;
- : float = 3.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This looks a lot what you'd expect from any language, but there are a
few differences that jump right out at you.

- We needed to type `;;` in order to tell the toplevel that it should
  evaluate an expression.  This is a pecularity of the toplevel that
  is not required in compiled code.
- After evaluating an expression, the toplevel spits out both the type
  of the result and the result itself.
- Function application in OCaml is syntactically unusual, in that
  function arguments are written out separated by spaces, rather than
  being demarcated by parens and commas.
- OCaml carefully distinguishes between `float`, the type for floating
  point numbers and `int`.  The types have different literals (`6.`
  instead of `6`) and different infix operators (`+.` instead of `+`),
  and OCaml doesn't do any automated casting between the types.  This
  can be a bit of a nuisance, but it has its benefits, since it
  prevents some classes of bugs that arise from confusion between the
  semantics of `int` and `float`.

We can also create variables to name the value of a given expression,
using the `let` syntax.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let x = 3 + 4;;
val x : int = 7
# let y = x + x;;
val y : int = 14
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

After a new variable is created, the toplevel tells us the name of the
variable, in addition to its type and value.

## Functions and Type Inference

The `let` syntax can also be used for creating functions:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let square x = x * x ;;
val square : int -> int = <fun>
# square (square 2);;
- : int = 16
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now that we're creating more interesting values, the types have gotten
more interesting too.  `int -> int` is a function type, in this case
indicating a function that takes an `int` and returns an `int`.  We
can also write functions that take multiple arguments:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let abs_diff x y =
    abs (x - y) ;;
val abs_diff : int -> int -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

and even functions that take other functions as arguments:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let abs_change f x =
    abs_diff (f x) x ;;
val abs_change : (int -> int) -> int -> int = <fun>
# abs_change square 10;;
- : int = 90
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This notation for multi-argument functions may be a little surprising
at first, but we'll explain where it comes from when we get to
function currying in Chapter {{{VARIABLES}}}.  For the moment, think
of the arrows as separating different arguments of the function, with
the type after the final arrow being the return value of the function.
Thus,

~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
int -> int -> int
~~~~~~~~~~~~~~~~~~~~~~~~~

describes a function that takes two `int` arguments and returns an
`int`, while

~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
(int -> int) -> int -> int
~~~~~~~~~~~~~~~~~~~~~~~~~

describes a function of two arguments where the first argument is
itself a function.

As the types we encounter get more complicated, you might ask yourself
how OCaml is able to determine these types, given that we didn't write
down any explicit type information.  It turns out that OCaml is able
to determine the type of a new expression using a technique called
_type-inference_, by which it infers the type of a new expression
based on what it already knows about the types of other related
variables.  For example, in `abs_change` above, the fact that
`abs_diff` is already known to take two integer arguments lets the
compiler infer that `x` is an `int` and that `f` returns an `int`.

Sometimes, there isn't enough information to fully determine the
concrete type of a given value.  Consider this function.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let first_if_true test x y =
    if (test x) then x else y;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`first_if_true` takes as its arguments a function `test`, and two
values, `x` and `y`, where `x` is to be returned if `(test x)`
evaluates to `true`, and `y` otherwise.  So what's the type of
`first_if_true`?  There are no obvious clues such as arithmetic
operators to tell you what the type of `x` and `y` are.  Indeed, it
seems like one could use this `first_if_true` on values of any type.
Indeed, if we look at the type returned by the toplevel:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
val first_if_true : ('a -> bool) -> 'a -> 'a -> 'a = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

we see that rather than choose a single concrete type, OCaml has
introduced a _type variable_ `'a` to express that the type is generic.
In particular, the type of the `test` argument is `('a -> bool)`,
which means that test is a one-argument function whose return value is
`bool`, and whose argument could be of any type `'a`.  But, whatever
type `'a` is, it has to be the same as the type of the other two
arguments, `x` and `y`.

This genericity means that we can write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let long_string s = String.length s > 6;;
val long_string : string -> bool = <fun>
# first_if_true long_string "short" "loooooong";;
- : string = "loooooong"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

And we can also write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let big_number x = x > 3;;
val big_number : int -> bool = <fun>
# first_if_true big_number 4 3;;
- : int = 4
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

But we can't mix and match two different concrete types for `'a` in
the same use of `first_if_true`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# first_if_true big_number "short" "loooooong";;
Characters 25-30:
  first_if_true big_number "short" "loooooong";;
                           ^^^^^^^
Error: This expression has type string but
    an expression was expected of type int
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

While the `'a` in the type of `first_if_true` can be instantiated as
any concrete type, it has to be the same concrete type in all of the
different places it appears.  This kind of genericity is called
_parametric polymorphism_, and is very similar to generics in C# and
Java.

<sidebar><title>Type errors vs exceptions</title>

There's a big difference in OCaml (and really in any compiled
language) between errors that are caught at compile time and those
that are caught at run-time.  It's better to catch errors as early as
possible in the development process, and compilation time is best of
all.

Working in the top-level somewhat obscures the difference between
run-time and compile time errors, but that difference is still there.
Generally, type errors, like this one:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# 3 + "potato";;
Characters 4-12:
  3 + "potato";;
      ^^^^^^^^
Error: This expression has type string but an expression was expected of type
         int
~~~~~~~~~~~~~~~~~~~~~~~~~~~

are compile-time errors, whereas division by zero, is a runtime error,
or an exception:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# 3 / 0;;
Exception: Division_by_zero.
~~~~~~~~~~~~~~~~~~~~~~~~~~~

One important distinction is that type errors will stop you whether or
not the offending code is ever actually executed.  Thus, you get an
error from typing in this code:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# if 3 < 4 then 0 else 3 + "potato";;
Characters 25-33:
  if 3 < 4 then 0 else 3 + "potato";;
                           ^^^^^^^^
Error: This expression has type string but an expression was expected of type
         int
~~~~~~~~~~~~~~~~~~~~~~~~~~~

but this code works fine.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# if 3 < 4 then 0 else 3 / 0;;
- : int = 0
~~~~~~~~~~~~~~~~~~~~~~~~~~~

</sidebar>

## Tuples, Lists, Options and Pattern-matching

### Tuples

So far we've encountered a handful of basic types like `int`, `float`
and `string` as well as function types like `string -> int`.  But we
haven't yet talked about any data structures.  We'll start by looking
at a particularly simple data structure, the tuple.  You can create a
tuple by joining values together with a comma:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let tup = (3,"three");;
val tup : int * string = (3, "three")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The type `int * string` corresponds to the set of pairs of `int`s and
`string`s.  For the mathematically inclined, the `*` character is used
because the space of all 2-tuples of type `t * s` corresponds to the
Cartesian product of `t` and `s`.

You can extract the components of a tuple using OCaml's
pattern-matching syntax. For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let (x,y) = tup;;
val x : int = 3
val y : string = "three"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`(x,y)` is the pattern, and it's used as the right-hand side of the
`let` binding.  Note that this operation lets us mint the new
variables `x` and `y`, each bound to different components of the value
being matched.

This is just a first taste of pattern matching.  Pattern matching
shows up in many contexts, and is a surprisingly powerful and
pervasive tool.

Here's another example: a function for computing the distance between
two points on the plane, where each point is represented as a pair of
`float`s.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let distance p1 p2 =
    let (x1,y1) = p1 in
    let (x2,y2) = p2 in
    sqrt ((x1 -. x2) ** 2. +. (y1 -. y2) ** 2)
;;
val distance : float * float -> float * float -> float = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can make this code more concise by doing the pattern matching on
the arguments to the function directly.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let distance (x1,y1) (x2,y2) =
    sqrt ((x1 -. x2) ** 2. +. sqr (y1 -. y2) ** 2.)
;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Lists

Where tuples let you combine a fixed number of items, potentially of
different types, lists let you hold any number of items of the same
type.  For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let languages = ["OCaml";"Perl";"C"];;
val languages : string list = ["OCaml"; "Perl"; "C"]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that you can't mix elements of different types on the same list,
as we did with tuples.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let numbers = [3;"four";5];;
Characters 17-23:
  let numbers = [3;"four";5];;
                   ^^^^^^
Error: This expression has type string but an expression was expected of type
         int
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can also use the operator `::` for adding elements to the front of
a list

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# "French" :: "Spanish" :: languages;;
- : string list = ["French"; "Spanish"; "OCaml"; "Perl"; "C"]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is creating a new list, not changing the list we started with, so
the definition of `languages` is unchanged.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# languages;;
- : string list = ["OCaml"; "Perl"; "C"]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The bracket notation for lists is just syntactic sugar for `::`.
Thus, the following declarations are all equivalent.  Note that `[]`
is used to represent the empty list.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# [1; 2; 3];;
- : int list = [1; 2; 3]
# 1 :: (2 :: (3 :: []));;
- : int list = [1; 2; 3]
# 1 :: 2 :: 3 :: [];;
- : int list = [1; 2; 3]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Basic list patterns

The elements of a list can be accessed through pattern-matching.  List
patterns have two key components: `[]`, which represents the empty
list, and `::`, which connects an element at the head of a list to the
remainder of the list, as you can see below.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let (my_favorite :: the_rest) = languages ;;
val my_favorite : string = "OCaml"
val the_rest : string list = ["Perl"; "C"]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

By pattern matching using `::`, we've broken off the first element of
`languages` from the rest of the list.  If you know Lisp or Scheme,
what we've done is the equivalent of using `car` and `cdr` to break
down a list.

If you tried the above example in the toplevel, you probably noticed
that I omitted a warning generated by the compiler.  Here's the full
output:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let (my_favorite :: the_rest) = languages ;;
Characters 5-28:
  let (my_favorite :: the_rest) = languages ;;
       ^^^^^^^^^^^^^^^^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
[]
val my_favorite : string = "OCaml"
val the_rest : string list = ["Perl"; "French"; "C"]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The warning comes because the compiler can't be certain that the
pattern match won't lead to a runtime error, and the warnings gives an
example of the problem, the empty list, `[]`.  Indeed, if we try to
use such a pattern-match on the empty list:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let (my_favorite :: the_rest) = [];;
Characters 5-28:
  let (my_favorite :: the_rest) = [];;
       ^^^^^^^^^^^^^^^^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
[]
Exception: (Match_failure "" 1 5).
~~~~~~~~~~~~~~~~~~~~~~~~~~~

we get a runtime error in addition to the compilation warning.

You can avoid these warnings, and more importantly make sure that your
code actually handles all of the possible cases, by using a `match`
statement.  Here's an example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is the syntax of a match statement.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
match <expr> with
| <pattern1> -> <expr1>
| <pattern2> -> <expr2>
| ...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The return value of the `match` is the result of evaluating the
right-hand side of the first pattern that matches the value of
`<expr>`.  As with `print_log_entry`, the patterns can mint new
variables, giving names to sub-components of the data structure being
matched.

#### Recursive list functions

_(yminsky: maybe we should kick this subsection to the full list
chapter?  This is getting long...)_

If we combine pattern matching with a recursive function call, we can
do things like define a function for summing the elements of a list.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let rec sum l =
    match l with
    | [] -> 0
    | hd :: tl -> hd + sum tl
  ;;
val sum : int list -> int
# sum [1;2;3;4;5];;
- : int = 15
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We had to add the `rec` keyword in the definition of `sum` to allow
for `sum` to refer to itself.  We can introduce more complicated list
patterns as well.  Here's a function for destuttering a list, _i.e._,
for removing sequential duplicates.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let rec destutter list =
    match list with
    | [] -> []
    | hd1 :: (hd2 :: tl) ->
      if hd1 = hd2 then destutter (hd2 :: tl)
      else hd1 :: destutter (hd2 :: tl)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Actually, the code above has a problem.  If you type it into
the top-level, you'll see this error:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
_::[]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is warning you that we've missed something, in particular that
our code doesn't handle one-element lists.  That's easy enough to fix
by adding another case to the match:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let rec destutter list =
    match list with
    | [] -> []
    | [hd] -> [hd]
    | hd1 :: (hd2 :: tl) ->
      if hd1 = hd2 then destutter (hd2 :: tl)
      else hd1 :: destutter (hd2 :: tl)
val destutter : 'a list -> 'a list = <fun>
# destutter ["hey";"hey";"hey";"man!"];;
- : string list = ["hey"; "man!"]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that in the above, we used another variant of the list pattern,
`[hd]`, to match a list with a single element.  We can do this to
match a list with any fixed number of elements, _e.g._, `[x;y;z]` will
match any list with exactly three elements, and will bind those
elements to the variables `x`, `y` and `z`.

#### The `List` module

So far, we've built up all of our list functions using pattern
matching and recursion.  But in practice, this isn't usually
necessary.  OCaml libraries are organized into _modules_, and there is
a module in Core called `List` which contains many useful functions
for dealing with lists.  For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# List.map ~f:String.length languages;;
- : int list = [5; 4; 6; 1]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here, we use the dot-notation to reference elements of the `List` and
`String` module.  `List.map` in particular is a function that takes a
list and a function for transforming elements of that list, and
returns to us a new list with the transformed elements.

There's another new piece of syntax to learn here: labeled arguments.
`String.length` is passed with the label, `~f`.  Labeled arguments are
arguments that are specified by name rather than position, which means
they can be passed in any order.  Thus, we could have written
`List.map languages ~f:String.length` instead of `List.map
~f:String.length languages`.  We'll see why labels are important in
Chapter {{{Functions}}}.

### Options

Another common data structure in OCaml is the `option`.  An `option`
is used to express that a value that might or might not be present.
For example,

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let divide x y =
    if y = 0 then None else Some (x/y)
val divide : int -> int -> int option = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here, `Some` and `None` are explicit tags, caled _type constructors_
that are used to build an optional value.  You can think of an
`option` as a specialized list that can only have zero or one element.

To get a value out of an option, we use pattern matching, as we did
with tuples and lists.  Consider the following simple function for
printing a log entry given an optional time and a message.  If no time
is provided (_i.e._, if the time is `None`), the current time is
computed and used in its place.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let print_log_entry maybe_time message =
    let time =
      match maybe_time with
      | Some x -> x
      | None -> Time.now ()
    in
    printf "%s: %s\n" (Time.to_string time) message
val print_log_entry : Time.t option -> string -> unit
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here, we again use a match statement for handling the two possible
states of an option.  It's worth noting that we don't necessarily need
to use an explicit `match` statement in this case.  We can instead use
some built in functions from the `Option` module, which, like the
`List` module for lists, is a place where you can find a large
collection of useful functions for owrking with options.

In this case, we can rewrite `print_log_entry` using `Option.value`,
which either returns the content of an option if the option is `Some`,
or a default value if the option is `None`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let print_log_entry maybe_time message =
    let time = Option.value ~default:(Time.now ()) maybe_time in
    printf "%s: %s\n" (Time.to_string time) message
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Options are important because they are the standard way in OCaml to
encode a value that might not be there.  Values in OCaml are
non-nullable, so if you have a function that takes an argument of type
`string`, then the compiler guarantees that, if the code compiles
successfully, then at run-time, that function will only be called with
awell-defined value of type `string`.  This is different from most
other languages, including Java and C#, where objects are by default
nullable, and whose type systems do little to defend from null-pointer
exceptions at runtime.

Given that in OCaml ordinary values are not nullable, you need some
other way of representing values that might not be there, and the
`option` type is the standard solution.

## Records and Variants

So far, we've only looked at data structures that were pre-defined in
the language, like lists and tuples.  But OCaml also allows us to
define new datatypes.  Here's a toy example of a datatype representing
a point in 2-dimensional space:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# type point2d = { x : float; y : float };;
type point2d = { x : float; y : float; }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`point2d` is a _record_ type, which you can think of as a tuple where
the individual fields are named, rather than being defined
positionally.  Record types are easy enough to construct:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let p = { x = 3.; y = -4. };;
val p : point2d = {x = 3.; y = -4.}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

And we can get access to the contents of these types using pattern
matching:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let magnitude { x = x; y = y } = sqrt (x ** 2. +. y ** 2.);;
val magnitude : point2d -> float = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the case where we want to name the value in a record field after
the name of that field, we can write the pattern match even more
tersely.  Instead of writing `{ x = x }` to name a variable `x` for
the value of field `x`, we can write `{ x }`.  Using this, we can
rewrite the magnitude function as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let magnitude { x; y } = sqrt (x ** 2. +. y ** 2.);;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can also use dot-notation for accessing record fields:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let distance v1 v2 =
     magnitude { x = v1.x -. v2.x; y = v1.y -. v2.y };;
val distance : point2d -> point2d -> float = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

And we can of course include our newly defined types as components in
larger types, as in the following types, each of which representing a
different geometric object.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# type circle = { center: point2d; radius: float } ;;
# type rect = { lower_left: point2d; width: float; height: float } ;;
# type segment = { endpoint1: point2d; endpoint2: point2d } ;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now, imagine that you want to combine multiple of these scene objects
together, say as a description scene containing multiple objects.  You
need some unified way of representing these objects together in a
single type.  One way of doing this is using a _variant_ type:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# type shape = | Circle of circle
               | Rect of rect
               | Segment of segment;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can think of a variant as a way of combining different types as
different possibilities.  The `|` character separates the different
cases of the variant (the first `|` is optional), and each case has a
tag (like `Circle`, `Rect` and `Segment`) to distinguish each case
from the other.  Here's how we might write a function for testing
whether a point is in the interior of one of a `shape list`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let is_inside_shape point shape =
     match shape with
     | Circle { center; radius } ->
       distance center point < radius
     | Rect { lower_left; width; height } ->
       point.x > lower_left.x && point.x < lower_left.x +. width
       && point.y > lower_left.y && point.y < lower_left.y +. height
     | Segment _ -> false
     ;;
val is_inside_shape : point2d -> shape -> bool = <fun>
# let is_inside_shapes point shapes =
     let point_is_inside_shape shape =
       is_inside_shape point shape
     in
     List.for_all shapes ~f:point_is_inside_shape
val is_inside_shapes : point2d -> shape list -> bool = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You might at this point notice that the use of `match` here is
reminiscent of how we used `match` with `option` and `list`.  This is
no accident: `option` and `list` are really just examples of variant
types that happen to be important enough to be defined in the standard
library (and in the case of lists, to have some special syntax).

## Mutation

All of our examples so far have been examples of mutation-free, or
_pure_ code.  This is typical of code in functional languages, which
tend to have a focus on so-called _pure_ code.  That said, OCaml has
good support for mutation, including standard mutable data structures
like arrays and hashtables.  For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let numbers = [| 1;2;3;4 |];;
val numbers : int array = [|1; 2; 3; 4|]
# numbers.(2) <- 4;;
- : unit = ()
# numbers;;
- : int array = [|1; 2; 4; 4|]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the above, the `ar.(i)` syntax is used for referencing the element
of an array, and the `<-` syntax is used for setting a mutable value.

Variable bindings in OCaml are always immutable, but datastructures
like arrays can be mutable.  In addition, record fields, which are
immutable by default can be declared as mutable.  Here's a small
example of a datastructure for mutable storing a running sum.  Here,
we've declared all the record fields as mutable.  Here

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# type running_sum = { mutable sum: float;
                       mutable sum_sq: float; (* sum of squares, for stdev *)
                       mutable samples: float; }
  let empty () = { sum = 0.; sum_sq = 0.; samples = 0. }
  let mean rsum = rsum.sum /. rsum.samples
  let stdev rsum =
     let square x = x *. x in
     sqrt (rsum.sum_sq /. rsum.samples -. square (rsum.sum /. rsum.samples))
  let update rsum x =
     rsum.sum <- rsum.sum +. x;
     rsum.sum_sq <- rsum.sum_sq +. x *. x;
     rsum.samples <- rsum.samples +. 1.
  ;;
# let rsum = empty ();;
val rsum : running_sum = {sum = 0.; sum_sq = 0.; samples = 0}
# List.iter [1.;3.;2.;-7.;4.;5.] ~f:(fun x -> update rsum x);;
- : unit = ()
# mean rsum;;
- : float = 1.33333333333333326
# stdev rsum;;
- : float = 1.61015297179882655
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can declare a single mutable value by using a `ref`, which is a
record type with a single mutable field that is defined in the
standard library.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let x = { contents = 0 };;
val x : int ref = {contents = 0}
# x.contents <- x.contents + 1;;
- : unit = ()
# x;;
- : int ref = {contents = 1}
~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are a handful of useful functions and operators defined for refs
to make them more convenient to work with.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let x = ref 0 ;; (* create a ref, i.e., { contents = 0 } *)
val x : int ref = {contents = 0}
# !x ;;            (* get the contents of a ref, i.e., x.contents *)
- : int = 0
# x := !x + 1 ;;   (* assignment, i.e., x.contents <- ... *)
- : unit = ()
# incr x ;;        (* increment, i.e., x := !x + 1 *)
- : unit = ()
# !x ;;
- : int = 2
~~~~~~~~~~~~~~~~~~~~~~~~~~~

## I/O

