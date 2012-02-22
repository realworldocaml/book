# Expressions, Variables and Functions

_(NOTE: this chapter is still very much in progress)_

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

OCaml function declarations come in multiple styles.  Perhaps the most
fundamental form is the definition of an anonymous functions using the
`fun` keyword:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# (fun x -> x + 1);;
- : int -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The above expression creates a one-argument function.  We can use the
function directly:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# (fun x -> x + 1) 7;;
- : int = 8
~~~~~~~~~~~~~~~~~~~~~~~~~~~

or we can name it, and then use it:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let plusone = (fun x -> x + 1);;
val plusone : int -> int = <fun>
# plusone 3;;
- : int = 4
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The declaration of `plusone` above is equivalent to the following
syntactic form that we saw in chapter {{TOUR}}:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let plusone x = x + 1;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is the most common and convenient way to declare a function, but
it's worth remembering that this is equivalent to creating and then
naming a function using the `fun` keyword.

### Multi-argument functions ###

OCaml of course also supports multi-argument functions, as we've
already seen.  Here's an example that came up in chapter {{TOUR}}.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let abs_diff x y = abs (x - y);;
val abs_diff : int -> int -> int = <fun>
# abs_diff 3 4;;
- : int = 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~

You may find the type signature of `abs_diff` a bit obscure at first.
To understand what's going on, let's rewrite `abs_diff` in an
equivalent form, using the `fun` keyword:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let abs_diff =
    (fun x -> (fun y -> abs (x - y)));;
val abs_diff : int -> int -> int = <fun>
# abs_diff 3 4;;
- : int = 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this rewrite `abs_diff` is really a function of one argument that
returns another function of one argument which in turn returns the
absolute value of the difference between the arguments taken by the
first and second functions.  It turns out the two formulations of
`abs_diff` are entirely equivalent.  `abs_diff` is what's called a
_curried function_.

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

The practice of applying some of the arguments of a curried function
to get a new function is called _partial application_.

The key to interpreting the type signature is the observation `->` is
left-associative.  Thus, we could parenthesize the type signature of
`abs_diff` as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
val abs_diff : int -> (int -> int)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the `fun` keyword supports its own syntactic sugar for
currying, so we could also have written `abs_diff` as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let abs_diff = (fun x y -> abs (x - y));;
~~~~~~~~~~~~~~~~~~~~~~~~~~~

You might worry that curried functions are terribly expensive, but
this is not an issue.  In OCaml, there is no penalty for calling a
curried function with all of its arguments.  (Partial application,
unsurprisingly, does have a cost.)

Currying is the standard way in OCaml of writing a multi-argument
function, but it's not the only way.  It's also possible to use the
different arms of a tuple as different arguments.  So, we could write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
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

### Declaring functions with `function` ###

Another way to define a function is using the `function` keyword.
Instead of having syntactic support for declaring curried functions,
`function` has built-in pattern matching.  Here's an example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
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

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
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

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let f ~foo:a ~bar:b = a + b
val f : foo:int -> bar:int -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

And the function can be called using the same convention:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# f ~foo:3 ~bar:10;;
- : int = 13
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In addition, OCaml supports _label punning_, meaning that you get to
drop the text after the `:` if the name of the label and the name of
the variable being used are the same.  Label punning works in both
function declaration and function invocation, as shown in these
examples:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
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

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
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

One last form of function argument that comes up in OCaml is
_optional_ arguments.  An optional argument, as you might imagine, is
an argument that the caller can choose whether or not to provide.
When an optional argument is absent, the function has to define some
sensible default behavior.  Consider the following example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
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

Note that optional arguments are passed in using the same syntax as
labeled arguments.  Also, similarly to labeled arguments, optional
arguments can be passed in in any order.

The behavior of substituting in a simple default is so common that it
has its own syntax.  Thus, we could rewrite the `concat` function as
follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let concat ?(sep="") x y =
     x ^ sep ^ y
  ;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~

One question that may not be clear is, when is an optional argument
erased, _i.e._, when is it determined that a given optional argument
is not going to be provided, and that the default value should be used
instead?

The rule for this is: an optional argument is erased as soon as the
first positional argument defined _after_ the optional argument is
passed in.  Thus, the following partial application of concat causes
the optional argument to disappear:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let prepend_foo = concat "foo";;
val prepend_foo : string -> string = <fun>
# prepend_foo "bar";;
- : string = "foobar"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

But if we had instead defined `concat` as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let concat x ?(sep="") y =
     x ^ sep ^ y
  ;;
val concat : string -> ?sep:string -> string -> string = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

then partial application of one argument would not cause the optional
argument to vanish.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let prepend_foo = concat "foo";;
val prepend_foo : ?sep:string -> string -> string = <fun>
# prepend_foo ~sep:"=" "bar";;;
- : string = "foo=bar"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

