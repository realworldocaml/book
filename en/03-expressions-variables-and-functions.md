# Expressions, Variables and Functions

In this section, we'll go into a bit more depth on the fundamental
building blocks of OCaml: expressions, variables and functions.

OCaml is an expression-oriented language, which is to say that most
OCaml code is structured as expressions to be evaluated rather than a
sequence of side-effecting statements to be executed.

OCaml is organized around the evaluation of expressions.


----------------------------------------------------------------
Type     Description
----     -----------
`bool`   Boolean values, `true` and `false` being the only inhabitants.

`int`    Integer values.  `int`s are signed, and the number of bits is
         platform dependent: 31 on 32-bit platforms, 63 on 64-bit
         platforms.

`char`   8-bit characters.

`string` Character strings.  Note that these are essentially
         byte-arrays, and as such have no direct support for unicode
         or other character encodings

----------------------------------------------------------------

Table: Basic types in OCaml


## Functions

OCaml function declaration comes in multiple styles, but perhaps the
most basic one is the definition of an anonymous functions using the
`fun` keyword:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# (fun x -> x + 1);;
- : int -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here, we're declaring a simple one-argument function.  We can
straightforwardly use such a function directly in an expression:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# (fun x -> x + 1) 7;;
- : int = 8
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can also name such a value, and then we get an ordinary named funciton

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let plusone = (fun x -> x + 1);;
val plusone : int -> int = <fun>
# plusone 3;;
- : int = 4
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is equivalent to the following form, which is the one that we
already saw in chapter {{TOUR}}:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let plusone x = x + 1;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can think of this final form as syntactic sugar for declaring a
function using `fun`.

### Multi-argument functions ###

We've already seen multi-argument functions.  Here's an example from
the previous chapter.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let abs_diff x y = abs (x - y);;
val abs_diff : int -> int -> int = <fun>
# abs_diff 3 4;;
- : int = 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~

You may find the type signature a bit obscure at first.  To understand
what's going on, let's rewrite `abs_diff` in an equivalent form, using
the `fun` keyword:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let abs_diff =
    (fun x -> (fun y -> abs (x - y)));;
val abs_diff : int -> int -> int = <fun>
# (abs_diff 3) 4;;
- : int = 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This definition and use of `abs_diff` are equivalent to the ones
above.  That's because `abs_diff` is really a function of one argument
that returns another function of one argument which in turn returns
the absolute value of the difference between the arguments taken by
the first and second functions.  This is what's called a _curried
function_.

Currying isn't just a theoretical issue: you can actually make use of
currying in practice.  Here's an example.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let dist_from_3 = abs_diff 3;;
val dist_from_3 : int -> int = <fun>
# dist_from_3 8;;
- : int = 5
# dist_from_3 (-1);;
- : int = 4
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This practice of applying some of the arguments of a curried function
to get a new function is called _partial application_.

The key to interpreting the type signature is the observation `->` is
left-associative.  Thus, we could parenthesize the type signature of
`abs_diff` as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
val abs_diff : int -> (int -> int)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the `fun` keyword supports currying directly, so we could
have written `abs_diff` as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let abs_diff = (fun x y -> abs (x - y));;
~~~~~~~~~~~~~~~~~~~~~~~~~~~

You might worry that curried functions are terribly expensive, but it
turns out not to be a problem.  Calling a curried function with all of
its arguments is quite fast.  Indeed, OCaml's function calls are very
fast, and generally more efficient than the function calls in C.

Currying is the standard way in OCaml of writing a multi-argument
function, but it's not the only way.  It's also possible to make the
different arguments be different parts of a tuple.  So, we could
write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let abs_diff (x,y) = abs (x - y)
val abs_diff : int * int -> int = <fun>
# abs_diff (3,4);;
- : int = 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~

It turns out OCaml handles this calling convention efficiently as
well.  In particular it does not generally have to allocate a tuple
just for the purpose of sending arguments to a tuple-style function.

There are small tradeoffs between these two styles, but most of the
time, once should stick to currying, since it's the more standard of
the two in the OCaml community.

### Declaring functions with `function` ###

Another way to define a function is using the `function` keyword.
Unlike `fun`, `function` does not have any syntactic sugar for curried
functions.  But what it does have is built-in pattern matching.
Here's an example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let some_or_zero = function
     | Some x -> x
     | None -> 0
  ;;
val some_or_zero : int option -> int = <fun>
# List.map ~f:some_or_zero [Some 3; None; Some 4];;
- : int list = [3; 0; 4]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can also combine the two styles, as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let some_or_default default = function
     | Some x -> x
     | None -> default
  ;;
# List.map ~f:(some_or_default 100) [Some 3; None; Some 4];;
- : int Core.Std.List.t = [3; 100; 4]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that we're using partial application in the above example, where
the function that gets mapped over the list is `some_or_default 100`.
We could have also written this with an explicit function around it:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# List.map ~f:(fun l -> some_or_default 100 l) [Some 3; None; Some 4];;
- : int Core.Std.List.t = [3; 100; 4]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This transformation, i.e., wrapping a function around an expression
instead of using partial application, is caled _eta-expansion_, and is
often a useful trick.  We'll talk more about eta-expansion in chapter
{{{VALUE RESTRICTION}}}

### Labeled Arguments ###

OCaml also supports functions with labeled arguments, which let you
identify a function argument by name rather than by position.
Functions with labeled arguments can be declared by putting a tilde in
front of the variable name in the definition of the function:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let f ~foo:a ~bar:b = a + b
val f : foo:int -> bar:int -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

And the function can be called using the same convention:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# f ~foo:3 ~bar:10;;
- : int = 13
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In addition, OCaml supports _label punning_, in both function
definition and function invocation, meaning that you get to drop the
`:` if the name of the label and the name of the variable are the
same.  So, we can write

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let f ~foo ~bar = foo + bar
val f : foo:int -> bar:int -> int = <fun>
# let foo = 3;;
# let bar = 4;;
# f ~foo ~bar;;
- : int = 7
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Labeled arguments are useful in a few different cases:

  - When defining a function with lots of arguments, because when you
    have enough arguments, names are easier to remember than positions.

  - For functions that have multiple arguments that might get confused
    with each other, particularly if they're of the same type.  For
    example, consider this signature for a function for creating a
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

    This makes the signature easier to read, and it also makes it
    easier to read code that uses this function.

  - Labeled arguments give you a way to assign a clear name and
    meaning to an argument whose type is otherwise less than
    informative.  For example, consider a function for creating a
    hashtable where the first argument is the initial size of the
    table, and the second argument is a flag which, when true,
    indicates that the hashtable will adjust its size down when its
    size is small.  The following signature is less than informative.

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
    val create_hashtable : int -> bool -> Hashtable.t
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    but with labeled arguments, we can make the intent much clearer:

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
    val create_hashtable : init_size:int -> allow_shrinking:bool -> Hashtable.t
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

  - Labeled arguments make it possible for the caller to decide which
    argument of a function to partially apply, whereas in ordinary
    curried functions, you can only partially apply the arguments in
    order, from first to last.  Labeled arguments also make it
    possible to place the arguments to a function in different orders,
    which is useful for functions like `List.map` where you often want
    to partially apply `List.map` with just the function, and at the
    same time mapping over a large function is easier to read if the
    functionis the last argument.

### Optional arguments ###
