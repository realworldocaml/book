# A Guided Tour

## Setting up

(Here we should give pointers to setting up OCaml, Core and rlwrap)

## Preliminaries

We're going to start off by introducing you to OCaml using the OCaml
toplevel, an interactive shell that lets you type in expressions and
evaluate them.  When you get to the point of running real programs,
you'll want to mostly leave the toplevel behind; but it's a great tool
for getting to know the language.

(Note that you'll want a tool like `rlwrap` available to give you the
ability to edit the command-line as you go.  Instructions for getting
`rlwrap` can be found in the [set up section](#setting-up).)

We'll start by launching OCaml from the command line and using it as a
simple calculator.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$ rlwrap ocaml
        Objective Caml version 3.12.1

# 3 + 4;;
- : int = 7
# 8 / 3;;
- : int = 2
# 3.5 +. 6.;;
- : float = 9.5
# sqrt 9.;;
- : float = 3.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This looks a lot what you'd expect from any language, but there are
some differences worth discussing:

- We needed to type `;;` in order to tell the toplevel that it should
  evaluate an expression.  Note that this is a pecularity of the
  toplevel, and is not required in compiled code.
- After evaluating the expression, the toplevel tells us two things:
  the type of the value that was returned, and the value itself.
- Function application in OCaml is syntactically different than you
  might expect from most other languages.  In particular, parens and
  commas are not needed to designate the arguments to a functions.
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

In addition to evaluating expressions, we can also name the value
returned by an expression using the `let` syntax.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let x = 3 + 4;;
val x : int = 7
# let y = x + x;;
val y : int = 14
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
After a new variable is created, the toplevel tells us the name of the
variable, in addition to its type and value.

## Functions and Type Inference

We can use a small variant on the `let` syntax for creating
functions.  

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let square x = x * x;;
val square : int -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We didn't write down an explicit return statement because OCaml
doesn't have one.  A function just returns the value that the body of
the function evaluates to.

Now that we're creating more interesting values, the types have gotten
more interesting too.  `int -> int` is a function type, in this case
indicating a function that takes an `int` and returns an `int`.  We
can also write functions that take multiple arguments:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let abs_diff x y = abs (x - y)
val abs_diff : int -> int -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

and functions that take other functions as arguments:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let abs_change f x =
    abs_diff (f x) x ;;
val abs_change : (int -> int) -> int -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This notation for multi-argument functions is a little surprising, but
we'll explain where it comes from when we get to function currying in
section {???}.  For the moment, think of the multiple arrows as
separating different arguments of the function, with the type after
the final arrow being the return value of the function.  Thus, `int ->
int -> int` describes a function that takes two `int` arguments and
returns an `int`, while `(int -> int) -> int -> int` describes a
function of two arguments where the first argument is itself a
funciton.

You might at this point ask yourself how all these types are
determined.  Type-inference is the answer.  Roughly speaking, OCaml
determines the type of an expression by leveraging what it knows about
the smaller expressions that it's made of.  So, for the `abs_diff`
function above, OCaml already knows that `(-)` has type `int -> int ->
int`.  From this, it can infer that `x` and `y` are `int`s, and that
the value fed to `abs` is an `int` as well.  It also knows that `abs`
has type `int -> int`, from which it can infer that the return value
of `abs_diff` is `int`.  Together, that's enough to determine the full
type of `abs_diff`.

Type-inference is an important feature because it allows you to write
terse code without giving up on the performance and correctness
benefits that you get from a static type system.

In all the types we've seen so far, all of the details of the type
have been clear from the type definitions.  But that's not always the
case.  Consider, for example, the following function:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let first_if_true test x y =
    if test x then x else y;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function takes a function called `test`, and two values, `x` and
`y`, where `x` is to be returned if `test` is `true` on `x`, and
otherwise `y`.  But what should the type of the function be?  In
particular, there's no clear way of inferring what the types of `x`
and `y` should be.  Let's look at what the toplevel returns for the
type:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
val first_if_true : ('a -> bool) -> 'a -> 'a -> 'a = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this example, we see something new: type variables.  `'a` doesn't
represent a particular type, but rather means that the function can be
used for any type `'a`.  So, we can use it on strings:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let long_string s = String.length s > 3;;
val long_string : string -> bool = <fun>
# first_if_true long_string "foo" "bar";;
- : string = "bar"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

And we can use it on integers:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let big_number x = x > 3;;
val big_number : int -> bool = <fun>
# first_if_true big_number 3 4;;
- : int = 4
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

But, we can't mix and match:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# first_if_true big_number "foo" "bar";;
Characters 25-30:
  first_if_true big_number "foo" "bar";;
                           ^^^^^
Error: This expression has type string but
    an expression was expected of type int
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is because, even though the `'a` in `('a -> bool) -> 'a -> 'a ->
'a` could be any type, it has to be the same type in all of the
different places it appears.  This kind of genericity in the type of a
function is called _parametric polymorphism_ in OCaml, and is very
similar to generics in C# and Java, except that generics are far more
natural and lightweight in OCaml.

## Lists, Tuples and Pattern-matching

So far, we've only encountered a handful of types: `int`, `float`,
`string`, along with function types.  But in order to see more
meaningful examples, we need ways of building up compound datatypes.
One fundamental example is that of a tuple.  Consider the following
simple function that 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# let quot_and_rem x y =
     (x / y, x mod y) ;;
val quot_and_rem : int -> int -> int * int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The return type, `int * int`, represents the set of 2-tuples of
`int`s.  (For the mathematically inclined, the `*` character is used
because the space of all 2-tuples is effectively the Cartesian product
of the constituent types)
