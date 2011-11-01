# A Guided Tour

## Setting up

(Here we should give pointers to setting up OCaml, Core and rlwrap)

## OCaml as a calculator

We're going to start off by introducing you to OCaml using the OCaml
toplevel, an interactive shell that lets you type in expressions and
evaluate them immediately.  When you get to the point of running real
programs, you'll want to leave the toplevel behind; but it's a great
tool for getting to know the language.

Let's to spin up the toplevel and open the `Core.Std` module, which
gives us access to Core's libraries.  Then we can take the toplevel
out for a spin as a simple calculator.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  evaluate an expression.  Note that this is a pecularity of the
  toplevel, and is not required in compiled code.
- After evaluating the expression, the toplevel spits out the type of
  the value that was returned and a representation of the value
  itself.
- Function application in OCaml is syntactically unusual, in that
  parens and commas are not needed to mark the arguments to a
  functions.
- OCaml separates strictly between `float`, the type for floating
  point numbers and `int`.  They have different literals (_e.g._, `6.`
  instead of `6`) and different infix operators (_e.g._, `+.` instead
  of `+`).  This can be a bit of a nuisance, but it has its benefits,
  since it makes it prevents some classes of bugs that arise from
  confusion between the semantics of `int` and `float`.

Another thing to be aware of is that infix operators like `+` can also
be used in ordinary prefix style by wrapping the operator in parens.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# (+) 3 4;;
- : int = 7
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can also create variables to name the value of a given expression,
using the `let` syntax.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let x = 3 + 4;;
val x : int = 7
# let y = x + x;;
val y : int = 14
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
After a new variable is created, the toplevel tells us the name of the
variable, in addition to its type and value.

## Functions and Type Inference

The `let` syntax can also be used for creating functions:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let square x =
    x * x ;;
val square : int -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now that we're creating more interesting values, the types have gotten
more interesting too.  `int -> int` is a function type, in this case
indicating a function that takes an `int` and returns an `int`.  We
can also write functions that take multiple arguments:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let abs_diff x y =
    abs (x - y) ;;
val abs_diff : int -> int -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

and even functions that take other functions as arguments:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let abs_change f x =
    abs_diff (f x) x ;;
val abs_change : (int -> int) -> int -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This notation for multi-argument functions may be a little surprising
at first, but we'll explain where it comes from when we get to
function currying in Chapter {???}.  For the moment, think of the
arrows as separating different arguments of the function, with the
type after the final arrow being the return value of the function.
Thus, 

~~~~~~~~~~~~~~~~~~~~~~~~~
int -> int -> int
~~~~~~~~~~~~~~~~~~~~~~~~~

describes a function that takes two `int` arguments and returns an
`int`, while 

~~~~~~~~~~~~~~~~~~~~~~~~~
(int -> int) -> int -> int
~~~~~~~~~~~~~~~~~~~~~~~~~

describes a function of two arguments where the first argument is
itself a function.

The types are quickly getting more complicated, and you might at this
point ask yourself how OCaml determines the types of a function.
Roughly speaking, OCaml determines the type of an expression by
leveraging what it knows about the elements of the expression, and
using those to infer a type for the overall expression.  This process
is called _type-inference_.  As an example, in `abs_change`, the fact
that `abs_diff` takes two integer arguments lets us infer that `x` is
an `int`, and that `f` returns an `int`.

Sometimes, the type-inference system doesn't have enough information
to fully determine the concrete type of a given value.  Consider this
example.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let first_if_true test x y =
    if test x then x else y;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function takes a function called `test`, and two values, `x` and
`y`, where `x` is to be returned if `test x` is `true`, and `y`
otherwise.  So what's the type of the function?  There are no obvious
clues such as arithmetic operators to tell you what the type of `x`
and `y` are.  Indeed, it seems like one could use this `first_if_true`
on values of different types.  In other words, it seems like this
function should be _generic_.  Indeed, if we look at the type returned
by the toplevel:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
val first_if_true : ('a -> bool) -> 'a -> 'a -> 'a = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

we see that rather than choose a particular type for the value being
tested, OCaml has introduced a _type variable_ `'a`.  This is how you
express in the type system that the value in question is generic, and
can be used with any type substituted in for `'a`.  So, we can
write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let long_string s = String.length s > 3;;
val long_string : string -> bool = <fun>
# first_if_true long_string "foo" "bar";;
- : string = "bar"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

And we can also write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let big_number x = x > 3;;
val big_number : int -> bool = <fun>
# first_if_true big_number 4 3;;
- : int = 4
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

But, we can't mix and match two different types for `'a` in the same
use of `first_if_true`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# first_if_true big_number "foo" "bar";;
Characters 25-30:
  first_if_true big_number "foo" "bar";;
                           ^^^^^
Error: This expression has type string but
    an expression was expected of type int
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is because, even though the `'a` in the type of `first_if_true`
could be any type, it has to be the same type in all of the different
places it appears.  This kind of genericity is called _parametric
polymorphism_, and is very similar to generics in C# and Java.

## Tuples, Options, Lists and Pattern-matching

### Tuples

So far, we've encountered a handful of basic types like `int`, `float`
and `string`.  We've also encountered function types, like `string ->
int`.  But we haven't yet talked about any datastructures.  We'll
start by looking at a particularly simple datastructure, the tuple.
Tuples are easy to create:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let tup = (3,"three")
val tup : int * string = (3, "three")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The type, `int * string` corresponds to set of pairs of `int`s and
`string`s.  (For the mathematically inclined, the `*` character is
used because the space of all 2-tuples of type `t * s` effectively
corresponds to the Cartesian product of `t` and `s`.)

You can extract the components of a tuple using OCaml's
pattern-matching syntax Here's a function for computing the distance
between two points on the plane, where each point is represented as a
pair of `float`s.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let distance p1 p2 =
    let (x1,y1) = p1 in
    let (x2,y2) = p2 in
    sqrt ((x1 -. x2) ** 2. +. (y1 -. y2) ** 2)
;;
val distance : float * float -> float * float -> float = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can make this code more concise by doing the pattern matching on
the arguments to the function directly:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let distance (x1,y1) (x2,y2) =
    sqrt ((x1 -. x2) ** 2. +. sqr (y1 -. y2) ** 2.)
;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is just a first taste of pattern matching.  Pattern matching
shows up in many contexts, and turns out to be a surprisingly powerful
tool.

### Options

Another common datastructure in OCaml is the `option`.  An `option` is
used to express that a value that might or might not be present.  For
example,

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let divide x y =
    if y = 0 then None else Some (x/y)
val divide : int -> int -> int option = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here, `Some` and `None` are explicit tags that are used to construct
an optional value.  To get a value out of an option, we again use
pattern matching.  Consider the following simple function for printing
a log entry given an optional time and a message.  If no time is
provided (_i.e._, if the time is `None`), the current time is computed
and used in its place.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let print_log_entry maybe_time message =
    let time =
      match maybe_time with
      | Some x -> x
      | None -> Time.now ()
    in
    printf "%s: %s\n" (Time.to_string_sec time) message
val print_log_entry : Time.t option -> string -> unit
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here, we use a new piece of syntax, the `match` statement, to do the
pattern matching.  A `match` statement lets you do a case analysis
driven by the shape of a datastructure, and it can be used for many
different datastructres in OCaml, 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
match <expr> with
| <pattern1> -> <expr1>
| <pattern2> -> <expr2>
| ...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Core also has a whole module full of useful functions for dealing with
options.  For example, we could rewrite `print_log_entry` using
`Option.value`, which returns the content of an option, or a default
value if the option is `None`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let print_log_entry maybe_time message =
    let time = Option.value ~default:(Time.now ()) maybe_time in
    printf "%s: %s\n" (Time.to_string time) message
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Lists

Tuples let you combine a fixed number of items, potentially of
different types, together in one datastructure.  Lists let you hold
any number of items of the same type in one datastructure.  For
example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let languages = ["OCaml";"Perl";"French";"C"];;
val languages : string list = ["Perl"; "OCaml"; "French"; "C"]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can access the elements of a list using pattern-matching.  List
patterns have two key components: `[]`, which represents the
empty-list, and `::`, which connects an element at the head of a list
to the remainder of the list.  Using these along with a recursive
function call, we can do things like define a function for summing the
elements of a list.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let rec sum l =
    match l with
    | [] -> 0
    | hd :: tl -> hd + sum tl
  ;;
val first : int list -> int
# sum [1;2;3;4;5];;
- : int = 15
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We had to add the `rec` keyword in the definition of `sum` to allow
for the recursive call.  We can introduce more complicated list
patterns as well.  Here's a function for destuttering a list, _i.e._,
for removing sequential duplicates.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

this is warning you that you missed a case, in particular, the case of
a list with one element.  That's easy enough to fix by adding another
case:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

But we don't have to build all of these functions ourselves using
pattern matching and recursion.  Just like there's an `Option` module
which contians useful functions for dealing with options, there's a
`List` module with useful functions for dealing with lists.  For
example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# List.map ~f:String.length languages;;
- : int list = [5; 4; 6; 1]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`List.map` is a function that takes a list and a function for
transforming elements of that list, and returns to us a new list with
the elements transformed.

There's a new piece of syntax to learn here: labeled arguments.  The
argument `double` is passed with a label, `~f`.  Labeled arguments can
be put anywhere on the argument list, so we could just as well have
written `List.map ~f:String.length languages` instead of `List.map
languages ~f:String.length`

