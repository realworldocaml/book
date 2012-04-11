# Variables and Functions

Variables and functions are fundamental ideas that show up in
virtually all programming languages.  But while these are familiar
topics, OCaml's variables and functions are different in subtle but
important ways from what you may have seen elsewhere.  Accordingly
we're going to spend a some time diving into the details of how these
concepts play out in OCaml.

## Variables

At its simplest, a variable is an identifier whose meaning is bound to
a particular value.  In OCaml these bindings are often introduced
using the `let` keyword.  When typed in at the prompt of the
interpreter, a let binding has the following syntax.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-syntax }
let <identifier> = <expr>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

As we'll see when we get to the module system in chapter
{{{MODULES}}}, this same syntax is used for top-level definitions in a
module.

Every variable binding has a _scope_, which is the portion of the code
that can access that binding.  The scope of a top-level let binding is
everything that follows it in the top-level session (or in the
remainder of the module).

Here's a simple example.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let x = 3;;
val x : int = 3
# let y = 4;;
val y : int = 4
# let z = x + y;;
val z : int = 7
~~~~~~~~~~~~~~~~~~~~~~~~~~~

`let` can also be used to create a variable binding whose scope is
limited to a particular expression, using the following syntax.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-syntax }
let <identifier> = <expr1> in <expr2>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This first evaluates _`expr1`_ and then evaluates _`expr2`_ with
_`identifier`_ bound to whatever value was produced by the evaluation
of _`expr1`_.  Here's how it looks in practice.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let languages = "OCaml,Perl,C++,C";;
val languages : string = "OCaml,Perl,C++,C"
# let dashed_languages =
     let language_list = String.split languages ~on:',' in
     String.concat ~sep:"-" language_list
  ;;
val dashed_languages : string = "OCaml-Perl-C++-C"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the scope of `language_list` is just the expression
`String.split languages ~on:','`, and is not available at the
top-level, as we can see if we try to access it now.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# language_list;;
Characters 0-13:
  language_list;;
  ^^^^^^^^^^^^^
Error: Unbound value language_list
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A let binding in an inner scope can _shadow_, or hide, the definition
from an outer scope.  So, for example, we could have written the
`dashed_languages` example as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let languages = "OCaml,Perl,C++,C";;
val languages : string = "OCaml,Perl,C++,C"
# let dashed_languages =
     let languages = String.split languages ~on:',' in
     String.concat ~sep:"-" languages
  ;;
val dashed_languages : Core.Std.String.t = "OCaml-Perl-C++-C"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This time, in the inner scope we called the list of strings
`languages` instead of `language_list`, thus hiding the original
definition of `languages`.  But once the definition of
`dashed_languages` is complete, the inner scope has closed and the
original definition of languages reappears.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# languages;;
- : string = "OCaml,Perl,C++,C"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

One common idiom is to use a series of nested `let`/`in` expressions
to build up the components of a larger compution.  Thus, we might
write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let area_of_ring inner_radius outer_radius =
     let pi = acos (-1.) in
     let area_of_circle r = pi *. r *. r in
     area_of_circle outer_radius -. area_of_circle inner_radius
  ;;
# area_of_ring 1. 3.;;
- : float = 25.1327412287183449
~~~~~~~~~~~~~~~~~~~~~~~~~~~

It's important not to confuse this sequence of let bindings with the
modification of a mutable variable.  How would `area_of_ring` be
different, for example, if we had instead written this purposefully
confusing bit of code:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let area_of_ring inner_radius outer_radius =
     let pi = acos (-1.) in
     let area_of_circle r = pi *. r *. r in
     let pi = 0. in
     area_of_circle outer_radius -. area_of_circle inner_radius
  ;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here, we redefined `pi` to be zero after the definition of
`area_of_circle`.  You might think that this would mean that the
result of the computation would now be zero, but you'd be wrong.  In
fact, the behavior of the function is unchanged.  That's because the
original definition of `pi` wasn't changed, it was just shadowed, so
that any subsequent reference to `pi` would see the new definition of
`pi` as zero.  But there is no later use of `pi`, so the binding
doesn't make a difference.  Indeed, if you type the example I gave
above into the toplevel, OCaml will warn you that the definition is
unused.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
Characters 126-128:
    let pi = 0. in
        ^^
Warning 26: unused variable pi.
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In OCaml, let bindings are immutable.  As we'll see in chapter
{{MUTABILITY}}, there are mutable values in OCaml, but no mutable
variables.

### Pattern matching and `let` ###

Another useful feature of let bindings is that they support the use of
patterns on the left-hand side of the bind.  Consider the following
code, which uses `List.unzip`, a function for converting a list of
pairs into a pair of lists.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let (ints,strings) = List.unzip [(1,"one"); (2,"two"); (3,"three")]
val ints : int list = [1; 2; 3]
val strings : string list = ["one"; "two"; "three"]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This actually binds two variables, one for each element of the pair.
Using a pattern in a let-binding makes the most sense for a pattern
that is _irrefutable_, _i.e._, where any value of the type in question
is guaranteed to match the pattern.  Tuple and record patterns are
irrefutable, but list patterns are not.  Indeed, the following pattern
match generates a warning because not all cases are covered.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let (hd::tl) = [1;2;3];;
Characters 4-12:
  let (hd::tl) = [1;2;3];;
      ^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
[]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

As a general matter, inexhaustive matches like the one above should be
avoided.

### `let`/`and` bindings ###

Another variant on the let binding is the use of `and` to join
multiple variable definitions into a single declaration.  For example,
we can write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let x = 100 and y = 3.5;;
val x : int = 100
val y : float = 3.5
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This can be useful when you want to create a number of new let
bindings at once, without having each definition affect the next.  So,
if we wanted to create new bindings that swapped the values of `x` and
`y`, we could write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let x = y and y = x ;;
val x : float = 3.5
val y : int = 100
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This use-case doesn't come up that often.  Most of the time that `and`
comes into play, it's used to define multiple mutually recursive
values, which we'll learn about later in the chapter.

Note that when doing a `let`/`and` style declaration, the order of
execution of the right-hand side of the binds is undefined by the
language definition, so one should not write code that relies on it.

## Functions ##

OCaml function declarations come in multiple styles.  The most basic
form is to create an _anonymous_ function using the `fun` keyword:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# (fun x -> x + 1);;
- : int -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The above expression creates a one-argument function, which can
straightforwardly be applied to an argument:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# (fun x -> x + 1) 7;;
- : int = 8
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Anonymous functions are quite convenient, particularly in a
higher-order context, _e.g._, when constructing a function to be
passed as an argument to another function.

We can create a named function using a let binding.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let plusone = (fun x -> x + 1);;
val plusone : int -> int = <fun>
# plusone 3;;
- : int = 4
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The declaration of `plusone` above is equivalent to the following
form, which we already saw in chapter {{TOUR}}:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let plusone x = x + 1;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is the most common and convenient way to declare a function, but
syntatic niceties aside, the two forms are entirely equivalent.

<sidebar>
<title>`let` and `fun`</title>

Functions and let bindings have a lot to do with each other.  In some
sense, you can think of the argument of a function as a variable being
bound to its argument.  Indeed, the following two expressions are
nearly equivalent:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# (fun x -> x + 1) 7;;
- : int = 8
# let x = 7 in x + 1;;
- : int = 8
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This connection is important, and will come up more when programming
in a monadic style, as we'll see in chapter {{ASYNC}}.

</sidebar>

### Multi-argument functions ###

OCaml of course also supports multi-argument functions.  Here's an
example that came up in chapter {{TOUR}}.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let abs_diff x y = abs (x - y);;
val abs_diff : int -> int -> int = <fun>
# abs_diff 3 4;;
- : int = 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~

You may find the type signature of `abs_diff` a bit obscure at first.
To understand what's going on, let's rewrite `abs_diff` in an
equivalent form, using the `fun` keyword:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let abs_diff =
    (fun x -> (fun y -> abs (x - y)));;
val abs_diff : int -> int -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This rewrite makes it explicit that `abs_diff` is actually a function
of one argument that returns another function of one argument, which
itself returns the absolute difference between the argument given to
the first function and the argument given to the second.  In other
words, `abs_diff` is a nested, or _curried_ function.  (Currying is
named after Haskell Curry, a famous logician who had a significant
impact on the design and theory of programming languages.)

The key to interpreting the type signature of a curried function is
the observation that `->` is right-associative.  The type signature of
`abs_diff` can therefore be parenthesized as follows to make the
currying more obvious without changing the meaning of the signature.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
val abs_diff : int -> (int -> int)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Currying is more than just a theoretical curiosity.  Here's an example
of how you can make use of currying.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let dist_from_3 = abs_diff 3;;
val dist_from_3 : int -> int = <fun>
# dist_from_3 8;;
- : int = 5
# dist_from_3 (-1);;
- : int = 4
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The practice of applying some of the arguments of a curried function
to get a new function is called _partial application_, and it is a
convenient way to mint new, specialized functions from more general
ones.

Note that the `fun` keyword supports its own syntactic sugar for
currying, so we could also have written `abs_diff` as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let abs_diff = (fun x y -> abs (x - y));;
~~~~~~~~~~~~~~~~~~~~~~~~~~~

You might worry that curried functions are terribly expensive, but
this is not an issue.  In OCaml, there is no penalty for calling a
curried function with all of its arguments.  (Partial application,
unsurprisingly, does have a small cost.)

Currying is the standard way in OCaml of writing a multi-argument
function, but it's not the only way.  It's also possible to use the
different arms of a tuple as different arguments.  So, we could write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let abs_diff (x,y) = abs (x - y)
val abs_diff : int * int -> int = <fun>
# abs_diff (3,4);;
- : int = 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~

OCaml handles this calling convention efficiently as well.  In
particular it does not generally have to allocate a tuple just for the
purpose of sending arguments to a tuple-style function.

There are small tradeoffs between these two styles, but most of the
time, once should stick to currying, since it's the default style in
the OCaml world.

### Recursive functions ###

In order to define a recursive function, you need to mark the let
binding as recursive with the `rec` keyword, as shown in this example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let rec find_first_stutter = function
     | [] | [_] ->
       (* only zero or one elements, so no repeats *)
       None
     | x :: y :: tl ->
       if x = y then Some x else find_first_stutter (y::tl)
val find_first_stutter : 'a list -> 'a option = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can also define multiple mutually recursive values by using `let
rec` and `and` together, as in this (gratuitiously inefficient)
example.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let rec is_even x =
    x = 0 || is_odd (x - 1)
  and is_odd x =
    is_even (x - 1)
val is_even : int -> bool = <fun>
val is_odd : int -> bool = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that in the above example, we take advantage of the fact that the
right hand side of the `||` is only evaluated if the left hand side
evaluates to false.

### Prefix and Infix operators ###

So far, we've seen examples of functions used in both prefix and infix
style:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Int.max 3 4;;  (* prefix *)
- : int = 4
# 3 + 4;;        (* infix  *)
- : int = 7
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In OCaml, functions can only be used infix if the name of the function
is chosen from one of a specialized set of identifiers called
_operators_.  An operator is any identifier that is a sequence of
characters from the following set

~~~~~~~~~~~~~~~~~~~~~~~~~~~
! $ % & * + - . / : < = > ? @ ^ | ~
~~~~~~~~~~~~~~~~~~~~~~~~~~~

or is one of a handful of pre-determined strings, including things
like `mod`, the modulus operator, and `lsl`, for "logical shift
right", which is a bit-shifting operation.

We can define (or redefine) the meaning of an operator as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let (+!) (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)
val ( +! ) : int * int -> int * int -> int *int = <fun>
# (3,2) +! (-2,4);;
- : int * int = (1,6)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that operators can be used in prefix style as well, if they are
put in parentheses:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# (+!) (3,2) (-2,4);;
- : int = (1,6)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The details of how the operator works are determined by the first
character of the operator.  This table describes how, and lists the
operators from highest to lowest precedence.

-------------------------------------------------------------
First character    Usage
-----------------  -------------------------------------------
`!` `?` `~`        Prefix and unary

`**`               Infix, right associative

`+` `-`            Infix, left associative

`@` `^`            Infix, right associative

`=` `<` `>` `|`    Infix, left associative
`&` `$`

-------------------------------------------------------------


### Declaring functions with `function` ###

Another way to define a function is using the `function` keyword.
Instead of having syntactic support for declaring curried functions,
`function` has built-in pattern matching.  Here's an example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let some_or_zero = function
     | Some x -> x
     | None -> 0
  ;;
val some_or_zero : int option -> int = <fun>
# List.map ~f:some_or_zero [Some 3; None; Some 4];;
- : int list = [3; 0; 4]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can also combine the different styles of function declaration
together, as in the following example where we declare a two argument
function with a pattern-match on the second argument.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let some_or_default default = function
     | Some x -> x
     | None -> default
  ;;
# List.map ~f:(some_or_default 100) [Some 3; None; Some 4];;
- : int Core.Std.List.t = [3; 100; 4]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Labeled Arguments ###

Up until now, the different arguments to a function have been
specified positionally, _i.e._, by the order in which the arguments
are passed to the function.  OCaml also supports labeled arguments,
which let you identify a function argument by name.  Functions with
labeled arguments can be declared by putting a tilde in front of the
variable name in the definition of the function:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let f ~foo:a ~bar:b = a + b
val f : foo:int -> bar:int -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

And the function can be called using the same convention:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# f ~foo:3 ~bar:10;;
- : int = 13
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In addition, OCaml supports _label punning_, meaning that you get to
drop the text after the `:` if the name of the label and the name of
the variable being used are the same.  Label punning works in both
function declaration and function invocation, as shown in these
examples:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let f ~foo ~bar = foo + bar
val f : foo:int -> bar:int -> int = <fun>
# let foo = 3;;
# let bar = 4;;
# f ~foo ~bar;;
- : int = 7
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Labeled arguments are useful in a few different cases:

  - When defining a function with lots of arguments.  When you have
    enough arguments, names are easier to remember than positions.

  - For functions that have multiple arguments that might get confused
    with each other, particularly if they're of the same type.  For
    example, consider this signature for a function for extracting a
    substring of another string.

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
    val substring: string -> int -> int -> string
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    where the two ints are the starting position and length of the
    substring to extract.  Labeled arguments can make this signature
    clearer:

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
    val substring: string -> pos:int -> len:int -> string
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    This improves the readability of both the signature and of client
    code that makes use of `substring`, and makes it harder to
    accidentally swap the position and the length.

  - Labeled arguments give you a way to assign a clear name and
    meaning to an argument whose type is otherwise less than
    informative.  For example, consider a function for creating a
    hashtable where the first argument is the initial size of the
    table, and the second argument is a flag which, when true,
    indicates that the hashtable will adjust its size down when its
    size is small.  The following signature is less than informative.

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
    val create_hashtable : int -> bool -> ('a,'b) Hashtable.t
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    but with labeled arguments, we can make the intent much clearer:

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
    val create_hashtable : init_size:int -> allow_shrinking:bool -> ('a,'b) Hashtable.t
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

  - Labeled arguments can be used to make a function signature more
    flexible.  For example, labeled arguments make it possible for
    the caller to decide which argument of a function to partially
    apply, whereas in ordinary curried functions, you can only
    partially apply the arguments in order from first to last.
    Labeled arguments also make it possible to place the arguments to
    a function in different orders, which is useful for functions like
    `List.map` where you often want to partially apply `List.map` with
    just the function, and at the same time mapping over a large
    function is easier to read if the function is the last argument.

One surprising gotcha about labeled arguments is that while order
doesn't matter when calling a function with labeled arguments, it does
matter in a higher-order context, _i.e._, when passing a labeled
argument to another function.  This is shown by the following example.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let apply_to_tuple f (first,second) = f ~first ~second;;
val apply_to_tuple : (first:'a -> second:'b -> 'c) -> 'a * 'b -> 'c = <fun>
# let divide ~second ~first = first / second;;
val divide : second:int -> first:int -> int = <fun>
# apply_to_tuple divide 3 4;;
Characters 15-21:
  apply_to_tuple divide 3 4;;
                 ^^^^^^
Error: This expression has type second:int -> first:int -> int
       but an expression was expected of type
         first:'a -> second:'b -> 'c -> 'd
~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Optional arguments ###

An optional argument is like a labeled argument that the caller can
choose whether or not to provide.  A function with an optional
argument must define a default for when the argument is absent.
Consider the following example of a string concatenation function with
an optionally specified separator.  Note that the `?` in front of an
argument is used to make the separator optional.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let concat ?sep x y =
     let sep = match sep with None -> "" | Some x -> x in
     x ^ sep ^ y
  ;;
val concat : ?sep:string -> string -> string -> string = <fun>
# concat "foo" "bar";; (* without the optional argument *)
- : string = "foobar"
# concat ~sep:":" "foo" "bar";; (* with the optional argument *)
- : string = "foo:bar"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Optional arguments can be passed in using the same syntax as labeled
arguments.  Also, similarly to labeled arguments, optional arguments
can be passed in in any order.

<sidebar>
<title>How are optional arguments inferred?</title>

One tricky aspect of labeled and optional arguments is the way in
which those arguments are inferred.  Consider the following example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let foo g x y = g ~x ~y ;;
val foo : (x:'a -> y:'b -> 'c) -> 'a -> 'b -> 'c = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In principle, it seems like the type first argument of `foo` could
have had a different order for the arguments (_e.g._ `y:'b ->
x:'a -> 'c`) or could have optional instead of labeled arguments
(_e.g._, `?y:'a -> x:'b -> 'c`).  OCaml disambiguates between these
cases by picking labeled arguments when it can, and by choosing the
order based on the order that is actually used.  If you try to use two
different orders in the same context, you'll get a compilation errro:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let foo g x y = g ~x ~y + g ~y ~x ;;
Characters 26-27:
  let foo g x y = g ~x ~y + g ~y ~x ;;
                            ^
Error: This function is applied to arguments
in an order different from other calls.
This is only allowed when the real type is known.
~~~~~~~~~~~~~~~~~~~~~~~~~~~

If, however, we put in an explicit type constraint, then we can
specify any compatible type.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let foo g x y = (g : ?y:'a -> x:'b -> int) ~x ~y + g ~y ~x;;
val foo : (?y:'a -> x:'b -> int) -> 'b -> 'a -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Type constraints are discussed in more detail in chapter {{???}}.

</sidebar>

The behavior of substituting in a default value is so common that it
has its own syntax.  Thus, we could rewrite the `concat` function as
follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let concat ?(sep="") x y = x ^ sep ^ y ;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Explicit passing of an optional argument ###

Sometimes you want to explicitly invoke an optional argument with a
concrete option, where `None` indicates that the argument won't be
passed in, and `Some` indicates it will.  You can do that as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# concat ?sep:None "foo" "bar";;
- : string = "foobar"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is particularly useful when you want to pass through an optional
argument from one function to another, leaving the choice of default
to the second function.  For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let uppercase_concat ?sep a b = concat ?sep (String.uppercase a) b ;;
val uppercase_concat : ?sep:string -> string -> string -> string =
  <fun>
# uppercase_concat "foo" "bar";;
- : string = "FOObar"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Erasure of optional arguments ###

One subtle aspect of optional arguments is the question of OCaml
decides to _erase_ an optional argument, _i.e._, to give up waiting
for an optional argument, and substitute in the default value?  Note
that, for ordinary labeled arguments, if you pass in all of the
non-labeled arguments, you're left with a partially applied function
that is still waiting for its labeled arguments.  _e.g._,

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let concat ~sep x y = x ^ sep ^ y ;;
val concat : sep:string -> string -> string -> string = <fun>
# concat "a" "b";;
- : sep:string -> string = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

So when should an optional argument be erased?

OCaml's rule is: an optional argument is erased as soon as the first
positional argument defined _after_ the optional argument is passed
in.  Thus, the following partial application of concat causes the
optional argument to disappear:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let prepend_foo = concat "foo";;
val prepend_foo : string -> string = <fun>
# prepend_foo "bar";;
- : string = "foobar"
# prepend_foo "bar" ~sep:":";;
Characters 0-11:
  prepend_foo "bar" ~sep:":";;
  ^^^^^^^^^^^
Error: This function is applied to too many arguments;
maybe you forgot a `;'
~~~~~~~~~~~~~~~~~~~~~~~~~~~

But if we had instead defined `concat` with the optional argument in
the second position:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let concat x ?(sep="") y = x ^ sep ^ y ;;
val concat : string -> ?sep:string -> string -> string = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

then application of the first argument would not cause the optional
argument to be erased.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let prepend_foo = concat "foo";;
val prepend_foo : ?sep:string -> string -> string = <fun>
# prepend_foo "bar";;;
- : string = "foobar"
# prepend_foo ~sep:"=" "bar";;;
- : string = "foo=bar"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

One oddity is that, if all arguments to a function are presented at
once, then erasure of optional arguments isn't applied until all of
the arguments are passed in.  Thus, this works:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# concat "a" "b" ~sep:"=";;
- : string = "a=b"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

but a well-placed pair of parenthesis fails.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# (concat "a" "b") ~sep:"=";;
Characters 0-16:
  (concat "a" "b") ~sep:"=";;
  ^^^^^^^^^^^^^^^^
Error: This expression is not a function; it cannot be applied
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The failure is a result of the fact that the expression `(concat "a"
"b")` has erased `concat`'s optional argument.

It's possible to define a function in such a way that the optional
argument can never be erased, by having no positional arguments
defined after the optional one.  This leads to a compiler warning:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let concat x y ?(sep="") = x ^ sep ^ y ;;
Characters 15-38:
  let concat x y ?(sep="") = x ^ sep ^ y ;;
                 ^^^^^^^^^^^^^^^^^^^^^^^
Warning 16: this optional argument cannot be erased.
val concat : string -> string -> ?sep:string -> string = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### When to use optional arguments ###

Optional arguments are very useful, but they're also easy to abuse.
The key advantage of optional arguments is that they let you write
functions with complex options that users can ignore most of the time,
only needing to think about them when they specifically want to invoke
those options.

The downside is that it's easy for the caller of a function to not
be aware that there is a choice to be made, and as a result end up
making the wrong choice by not doing anything.  Optional arguments
really only make sense when the extra concision of omitting the
argument overwhelms the corresponding loss of explicitness.

This means that rarely used functions should not have optional
arguments.  A good rule of thumb for optional arguments is that you
should never use an optional argument for internal functions of a
module, only for functions that are exposed to users of a module.
