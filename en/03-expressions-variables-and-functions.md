# Variables and Functions

_(yminsky: still don't have a decent introceptive)_ 





Variables in OCaml are different from variables in most other
languages in that OCaml's variables are immutable.  At its simplest, a
variable is an identifier whose meaning is bound to a particular
value.  In OCaml these bindings are introduces using the `let`
keyword, which at the top-level of a module has the following syntax.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-syntax }
let <identifier> = <expr>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Every variable binding has a _scope_, which is the portion of the code
which includes this binding in its evaluation environment.  The scope
of a top-level let binding is everything that follows it.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let x = 3;;
val x : int = 3
# let y = 4;;
val y : int = 4
# let z = x + y;;
val z : int = 7
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let bindings can also be used to create a variable binding whose scope
is limited to a particular expressions, using the following syntax.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-syntax }
let <identifier> = <expr1> in <expr2>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This first evaluates `<expr1>`, and then evaluates `<expr2>` in an
environment that has an extra variable binding, in particular, the
binding of `<identifier>` to whatever `<expr1>` evaluated to.

In this form, multiple let bindings can be nested, like this:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let x = 3 in
  let y = 4 in
  x + y
;;
- : int = 7
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that nested bindings can _shadow_, or hide, previous bindings.
Thus, we can write

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let x = 1 in
  let x = x + x in
  let x = Int.to_string (x + x) in
  x ^ x;;
- : string = "44"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

It's important not to confuse shadowing of variables with assignment,
_i.e._, mutating a variable by assigning a new definition to it.
Consider the following function.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let x = 3 in
  let add_to_x y = x + y in
  let x = 4 in
  add_to_x 0
;;
- : int = 3
~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the second let binding of x were in fact an assignment, then you
would expect `add_to_x 0` to return `4`.  Instead, it returns `3`,
because the `x` that `add_to_x` refers to is still there, unchanged,
even after the new binding of `x` to `4` is created.

Here's another demonstration of how let bindings differ from
assignments.  In the following example, the second binding of `x` is
only visible within the small scope of a particular sub-expression, in
particular, the sub-expression that makes up the right-hand side of
the definition of `y`.  So, when the definition of `y` is complete, we
see that the definition of `x` is unaffected.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let x = 3 in
  let y =
    let x = 2 in
    x + x
  in
  x + y
- : int = 7
~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Pattern matching in let bindings ###

Another useful feature of let bindings is that they support the use of
simple patterns on the left-hand side of the bind.  Consider the
following code, which uses `List.unzip`, a function for converting a
list of pairs to a pair of lists.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let (ints,strings) = List.unzip [(1,"one"); (2,"two"); (3,"three")]
val ints : int Core.Std.List.t = [1; 2; 3]
val strings : string Core.Std.List.t = ["one"; "two"; "three"]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This actually binds two variables, one for each element of the pair.
Using a pattern in a let-binding makes the most sense for a pattern
that is _irrefutable_, i.e., where everything of the type in question
must match the pattern.  Tuple and record patterns are immutable, but
list patterns are not.  Here's an example of a list pattern match that
generates a warning because not all cases are covered.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let (hd::tl) = [1;2;3];;
Characters 4-12:
  let (hd::tl) = [1;2;3];;
      ^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
[]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

### `let`/`and` bindings ###

Another form of let binding that comes up on occasion is where you
bind multiple arguments in parallel in a single declaration.  For
example, we can write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let x = 100 and y = 3.5;;
val x : int = 100
val y : float = 3.5
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This can be useful when you want to create a number of new definitions
at once, without having each definition affect the next.  So, if we
wanted to create new bindings that swapped the values of `x` and `y`,
we could write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let x = y and y = x ;;
val x : float = 3.5
val y : int = 100
~~~~~~~~~~~~~~~~~~~~~~~~~~~

But this use-case doesn't come up that often.  Most of the time that
`and` comes into play, it's as part defining multiple mutually
recursive values, which we'll learn about later in the chapter.

Note that when doing a `let`/`and` style declaration, the order of
execution of the right-hand side of the definitions is undefined.

## Functions ##

OCaml function declarations come in multiple styles.  Perhaps the most
fundamental form is the definition of an anonymous functions using the
`fun` keyword:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# (fun x -> x + 1);;
- : int -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The above expression creates a one-argument function.  We can use the
function directly:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# (fun x -> x + 1) 7;;
- : int = 8
~~~~~~~~~~~~~~~~~~~~~~~~~~~

<sidebar>
<title>Connections between `let` and `fun`</title>

Functions and let bindings have a lot to do with each other.  In some
sense, you can think of the argument of a function as a variable being
bound.  The following two code snippets are nearly equivalent:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# (fun x -> x + 1) 7;;
- : int = 8
# let x = 7 in x + 1;;
- : int = 8
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This connection is important, and will come up more when programming
in a monadic style, as we'll see in chapter {{ASYNC}}
</sidebar>

or we can name it, and then use it:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let plusone = (fun x -> x + 1);;
val plusone : int -> int = <fun>
# plusone 3;;
- : int = 4
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The declaration of `plusone` above is equivalent to the following
syntactic form that we saw in chapter {{TOUR}}:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let plusone x = x + 1;;
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is the most common and convenient way to declare a function, but
it's worth remembering that this is equivalent to creating and then
naming a function using the `fun` keyword.

### Multi-argument functions ###

OCaml of course also supports multi-argument functions, as we've
already seen.  Here's an example that came up in chapter {{TOUR}}.

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

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
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

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
val abs_diff : int -> (int -> int)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the `fun` keyword supports its own syntactic sugar for
currying, so we could also have written `abs_diff` as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let abs_diff = (fun x y -> abs (x - y));;
~~~~~~~~~~~~~~~~~~~~~~~~~~~

You might worry that curried functions are terribly expensive, but
this is not an issue.  In OCaml, there is no penalty for calling a
curried function with all of its arguments.  (Partial application,
unsurprisingly, does have a cost.)

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
       (* only one or two elements, so no repeats *)
       None
     | x :: y :: tl ->
       if x = y then Some x else find_first_stutter (y::tl)
val find_first_stutter : 'a list -> 'a option = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can also define multiple mutually recursive values by using `let
rec` and `and` together, as in this example.

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
style.  Most of the time, functions are used in prefix style, for example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
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

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let (+!) x y = x + y * y;;
val ( +! ) : int -> int -> int = <fun>
# 3 +! 4;;
- : int = 19
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that operators can be used in prefix style as well, if they are
put in parens:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
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

One pattern to be particularly leery of is the pattern of passing
optional arguments from one function to another, because it's easy to
forget an optional argument part way down a chain.  Note that the
following code compiles without complaint, even though the optional
argument passed into `uppercase_concat` is never passed into `concat`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let uppercase_concat ?sep a b = concat (String.uppercase a) b ;;
val uppercase_concat :
  ?sep:'a -> Core.Std.String.t -> string -> ?sep:string -> string = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

