# Expressions, Variables and Functions

_(NOTE: this chapter is still very much in progress)_

In this section, we'll go into a bit more depth on the fundamental
building blocks of OCaml: expressions, variables and functions.

OCaml is an expression-oriented language, which is to say that most
OCaml code is structured as expressions to be evaluated rather than a
sequence of side-effecting statements to be executed.

## Functions

### Declaring functions ###

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

### Prefix and Infix operators ###

So far, we've seen examples of functions used in both prefix and infix
style.  Most of the time, functions are used in prefix style, for example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# Int.max 3 4;;
- : int = 4
~~~~~~~~~~~~~~~~~~~~~~~~~~~

But there are a specialized set of identifiers called _operators_
which can be used for definining infix functions as well as prefix
functions that bind more tightly than simple function application.  An
operator is defined as being any identifier which is a sequence of
characters from the following set:

~~~~~~~~~~~~~~~~~~~~~~~~~~~
! $ % & * + - . / : < = > ? @ ^ | ~
~~~~~~~~~~~~~~~~~~~~~~~~~~~

or one of a handful of fixed strings that are defined to be operators,
including things like `mod`, the modulus operator, and `lsl`, for
"logical shift right", which is a bit-shifting operation.

We can define (or redefine) an operator as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let (+!) x y = x + y * y;;
val ( +! ) : int -> int -> int = <fun>
# 3 +! 4;;
- : int = 19
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that operators can be used in prefix style as well, if they are
put in parens:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# (+!) 3 4;;
- : int = 19
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
_optional_ arguments.  An optional argument is basically a form of
labeled argument that the caller can choose whether or not to provide.
A function with an optional argument must define a default for when
the argument is absent.  Consider the following example of a string
concatenation function with an optionally specified separator.  Note
that the `?` in front of an argument is used to make the separator
optional.

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

As you can see, optional arguments are passed in using the same syntax
as labeled arguments.  Also, similarly to labeled arguments, optional
arguments can be passed in in any order.

The behavior of substituting in a default value is so common that it
has its own syntax.  Thus, we could rewrite the `concat` function as
follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let concat ?(sep="") x y = x ^ sep ^ y ;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Explicit passing of an optional argument ####

Sometimes you want to explicitly invoke an optional argument with a
concrete option, where `None` indicates that the argument won't be
passed in, and `Some` indicates it will.  You can do that as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# concat ?sep:None "foo" "bar";;
- : string = "foobar"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is particularly useful when you want to pass through an optional
argument from one function to another, leaving the choice of default
to the second function.  For example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let uppercase_concat ?sep a b = concat ?sep (String.uppercase a) b ;;
val uppercase_concat : ?sep:string -> string -> string -> string =
  <fun>
# uppercase_concat "foo" "bar";;
- : string = "FOObar"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Erasure of optional arguments ####

One subtle aspect of optional arguments is the question of OCaml
decides to _erase_ an optional argument, _i.e._, to give up waiting
for an optional argument, and substitute in the default value?  Note
that, for ordinary labeled arguments, if you pass in all of the
non-labeled arguments, you're left with a partially applied function
that is still waiting for its labeled arguments.  _e.g._,

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
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

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
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

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let concat x ?(sep="") y = x ^ sep ^ y ;;
val concat : string -> ?sep:string -> string -> string = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

then application of the first argument would not cause the optional
argument to be erased.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
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

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# concat "a" "b" ~sep:"=";;
- : string = "a=b"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

but a well-placed pair of parenthesis fails.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
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

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let concat x y ?(sep="") = x ^ sep ^ y ;;
Characters 15-38:
  let concat x y ?(sep="") = x ^ sep ^ y ;;
                 ^^^^^^^^^^^^^^^^^^^^^^^
Warning 16: this optional argument cannot be erased.
val concat : string -> string -> ?sep:string -> string = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### When to use optional arguments ####

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

One pattern to be particularly leery of is the pattern of passing
optional arguments from one function to another, because it's easy to
forget an optional argument part way down a chain.  Note that the
following code compiles without complaint, even though the optional
argument passed into `uppercase_concat` is never passed into `concat`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let uppercase_concat ?sep a b = concat (String.uppercase a) b ;;
val uppercase_concat :
  ?sep:'a -> Core.Std.String.t -> string -> ?sep:string -> string = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Expressions ##

OCaml has a handful of basic forms around which OCaml expressions are
built.  We'll describe those forms below.

### Function application ###

This is perhaps the simplest thing that you can do in OCaml.  The
basic form is:

~~~~~~~~~~~~~~~~~~~~~~~~~~~
<function> <arg1> [<arg2> ...]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Functions come in both prefix and infix styles:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# min 3 4;;
- : int = 3
# 3 > 4;;
- : bool = false
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here,

### Let binding ###
