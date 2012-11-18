# Variables and Functions

Variables and functions are fundamental ideas that show up in
virtually all programming languages.  But OCaml has a different take
on these basic concepts, and so we'll spend some time digging into the
details of OCaml's variables and functions differ from what you may
have seen elsewhere.

## Variables

At its simplest, a variable is an identifier whose meaning is bound to
a particular value.  In OCaml these bindings are often introduced
using the `let` keyword.  When typed in at the prompt of the
interpreter, a let binding has the following syntax.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-syntax }
let <identifier> = <expr>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

As we'll see when we get to the module system in Chapter
{{{files-modules-and-programs}}}, this same syntax is used for
top-level definitions in a module.

Every variable binding has a _scope_, which is the portion of the code
that can refer to that binding.  The scope of a top-level let binding
is everything that follows it in the top-level session (or in the
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
`String.concat ~sep:"-" language_list`, and is not available at the
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
to build up the components of a larger computation.  Thus, we might
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
{{{imperative-programming}}}, there are mutable values in OCaml, but
no mutable variables.

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
irrefutable, but list patterns are not.  Consider the following code
that implements a function for up-casing the first element of a
comma-separate list.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let upcase_first_entry line =
     let (key :: values) = String.split ~on:',' line in
     String.concat ~sep:"," (String.uppercase key :: values)
  ;;
      Characters 40-53:
       let (key :: values) = String.split ~on:',' line in
            ^^^^^^^^^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
[]
val upcase_first_entry : string -> string = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This case can't really come up in practice, because `String.split`
always returns a list with at least one element.  But the compiler
doesn't know this, and so it emits the warning.  It's generally better
to use a match statement to handle such cases explicitly:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let upcase_first_entry line =
     match String.split ~on:',' line with
     | [] -> assert false (* String.split returns at least one element *)
     | first :: rest -> String.concat ~sep:"," (String.uppercase first :: rest)
  ;;
val upcase_first_entry : string -> string = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

OCaml being a functional language, it's no surprise that functions are
an important and pervasive element of programming in OCaml.  Indeed,
we've seen functions pop up already in many of the examples we've
looked at thus far.  But while we've introduced the basics of
functions, we're now going to cover them in more depth, starting from
the foundations.

### Anonymous Functions ###

We'll start by looking at the most basic form of OCaml function, the
_anonymous_ function.  Anonymous functions are declared using the
`fun` keyword, as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# (fun x -> x + 1);;
- : int -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Anonymous functions aren't named, but they can be used for many
different purposes nonetheless.  You can, for example, apply an
anonymous function to an argument:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# (fun x -> x + 1) 7;;
- : int = 8
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Or pass it to another function.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# List.map ~f:(fun x -> x + 1) [1;2;3];;
- : int list = [2; 3; 4]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Or even stuff then into a datastructure.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let increments = [ (fun x -> x + 1); (fun x -> x + 2) ] ;;
val increments : (int -> int) list = [<fun>; <fun>]
# List.map ~f:(fun f -> f 5) increments;;
- : int list = [6; 7]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

It's worth stopping for a moment to puzzle this example out, since
this kind of higher-order use of functions can be a bit obscure at
first.  The first thing to understand is the function `(fun f -> f
5)`, which takes a function as its argument and applies that function
to the number `5`.  The invocation of `List.map` applies `(fun f -> f
5)` to the elements of the `increments` list (which are themselves
functions) and returns the list containing the results of these
function applications.

The key thing to understand is that functions are ordinary values in
OCaml, and you can do everything with them that you'd do with an
ordinary value, including passing them to and returning them from
other functions and storing them in datastructures.  We even name
functions in the same way that we name other values, by using a let
binding.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let plusone = (fun x -> x + 1);;
val plusone : int -> int = <fun>
# plusone 3;;
- : int = 4
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Defining named functions is so common that there is some built in
syntactic sugar for it.  Thus, we can write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let plusone x = x + 1;;
val plusone : int -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is the most common and convenient way to declare a function, but
syntactic niceties aside, the two styles of function definition are
entirely equivalent.

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

You may find the type signature of `abs_diff` with all of its arrows a
little hard to parse.  To understand what's going on, let's rewrite
`abs_diff` in an equivalent form, using the `fun` keyword:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let abs_diff =
    (fun x -> (fun y -> abs (x - y)));;
val abs_diff : int -> int -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

This rewrite makes it explicit that `abs_diff` is actually a function
of one argument that returns another function of one argument, which
itself returns the final computation.  Because the functions are
nested, the inner expression `abs (x - y)` has access to both `x`,
which was captured by the first function application, and `y`, which
was captured by the second one.

This style of function is called a _curried_ function.  (Currying is
named after Haskell Curry, a famous logician who had a significant
impact on the design and theory of programming languages.)  The key to
interpreting the type signature of a curried function is the
observation that `->` is right-associative.  The type signature of
`abs_diff` can therefore be parenthesized as follows.  This doesn't
change the meaning of the signature, but it makes it easier to see how
the currying fits in.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
val abs_diff : int -> (int -> int)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Currying is more than just a theoretical curiosity.  You can make use
of currying to specialize a function by feeding in some of the
arguments.  Here's an example where we create a specialized version of
`abs_diff` that measures the distance of a given number from `3`.

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

Note that the `fun` keyword supports its own syntactic sugar for
currying, so we could also have written `abs_diff` as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let abs_diff = (fun x y -> abs (x - y));;
~~~~~~~~~~~~~~~~~~~~~~~~~~~

You might worry that curried functions are terribly expensive, but
this is not the case.  In OCaml, there is no penalty for calling a
curried function with all of its arguments.  (Partial application,
unsurprisingly, does have a small extra cost.)

Currying is not the only way of writing a multi-argument function in
OCaml.  It's also possible to use the different arms of a tuple as
different arguments.  So, we could write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let abs_diff (x,y) = abs (x - y)
val abs_diff : int * int -> int = <fun>
# abs_diff (3,4);;
- : int = 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~

OCaml handles this calling convention efficiently as well.  In
particular it does not generally have to allocate a tuple just for the
purpose of sending arguments to a tuple-style function.

There are small tradeoffs between these two approaches, but most of
the time, one should stick to currying, since it's the default style
in the OCaml world.

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
rec` and `and` together, as in this (gratuitously inefficient)
example.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let rec is_even x =
    if x = 0 then true else is_odd (x - 1)
  and is_odd x =
    if x = 0 then false else is_even (x - 1) 
 ;;
val is_even : int -> bool = <fun>
val is_odd : int -> bool = <fun>
# List.map ~f:is_even [0;1;2;3;4;5];;
- : bool Core.Std.List.t = [true; false; true; false; true; false]
# List.map ~f:is_odd [0;1;2;3;4;5];;
- : bool Core.Std.List.t = [false; true; false; true; false; true]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that in the above example, we take advantage of the fact that the
right hand side of `||` is evaluated lazily, only being executed if
the left hand side evaluates to false.

### Prefix and Infix operators ###

So far, we've seen examples of functions used in both prefix and infix
style:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# Int.max 3 4;;  (* prefix *)
- : int = 4
# 3 + 4;;        (* infix  *)
- : int = 7
~~~~~~~~~~~~~~~~~~~~~~~~~~~

You might not have thought of the second example as an ordinary
function, but it very much is.  Infix operators like `+` really only
differ syntactically from other functions.  In fact, if we put
parenthesis around an infix operator, you can use it as an ordinary
prefix function.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# (+) 3 4;;
- : int = 7
# List.map ~f:((+) 3) [4;5;6];;
- : int list = [7; 8; 9]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the second expression above, we've partially applied `(+)` to gain
a function that increments its single argument by `3`, and then
applied that to all the elements of a list.

A function is treated syntactically as an operator if the name of that
function is chosen from one of a specialized set of identifiers.  This
set includes any identifier that is a sequence of characters from the
following set

~~~~~~~~~~~~~~~~~~~~~~~~~~~
! $ % & * + - . / : < = > ? @ ^ | ~
~~~~~~~~~~~~~~~~~~~~~~~~~~~

or is one of a handful of pre-determined strings, including `mod`, the
modulus operator, and `lsl`, for "logical shift left", a bit-shifting
operation.

We can define (or redefine) the meaning of an operator as follows.
Here's an example of a simple vector-addition operator on int pairs.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let (+!) (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)
val ( +! ) : int * int -> int * int -> int *int = <fun>
# (3,2) +! (-2,4);;
- : int * int = (1,6)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The syntactic role of an operator work is determined by its first
character.  This table describes how, and lists the operators from
highest to lowest precedence.

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

Here's an example of a very useful operator that's defined in Core,
following these rules.  Here's the definition:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let (|!) x f = f x ;;
val ( |! ) : 'a -> ('a -> 'b) -> 'b = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

It's not quite obvious at first what the purpose of this operator is:
it just takes some value and a function, and applies the function to
the value.  But its utility is clearer when you see it in action.  It
works as a kind of sequencing operator, similar in spirit to using
pipe in the UNIX shell.  So, for example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let drop_zs string =
    String.to_list string
    |! List.filter ~f:(fun c -> c <> 'z')
    |! String.of_char_list
  ;;
val drop_zs : string -> string = <fun>  
# drop_zs "lizkze UNIX zzpipes wizth tzzypzzes";;
- : string = "like UNIX pipes with types"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that `|!` works here because it is left-associative.  If it were
right associative, it wouldn't be doing the right thing at all.
Indeed, let's see what happens if we try using a right associative
operator, like (^!).

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let (^!) = (|!);;
val ( ^! ) : 'a -> ('a -> 'b) -> 'b = <fun>
# let drop_zs string =
    String.to_list string
    ^! List.filter ~f:(fun c -> c <> 'z')
    ^! String.of_char_list
  ;;
        Characters 96-115:
      ^! String.of_char_list
         ^^^^^^^^^^^^^^^^^^^
Error: This expression has type char list -> string
       but an expression was expected of type
         (char list -> char list) -> 'a
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The above type error is a little bewildering at first glance.  What's
going on is that, because `^!` is right associative, the operator is
trying to feed the value `List.filter ~f:(fun -> c <> 'z')` to the
function `String.of_char_list`.  But `String.of_char_list` expects a
list of characters as its input, not a function.

The type error aside, this example highlights the importance of
choosing the operator you use with care, particularly with respect to
associativity.

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

This is equivalent to combining a `fun` with `match`, as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let some_or_zero num_opt =
    match num_opt with
    | Some x -> x
    | None -> 0
  ;;
val some_or_zero : int option -> int = <fun>
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

Also, note the use of partial application to generate the function
passed to `List.map`

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

  - When defining a function with lots of arguments.  Beyond a certain
    number, arguments are easier to remember by name than by position.

  - When defining functions that have multiple arguments that might
    get confused with each other.  This is most at issue when the
    arguments are of the same type.  For example, consider this
    signature for a function for extracting a substring of another
    string.

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

  - When the meaning of a particular argument is unclear from the type
    alone.  For example, consider a function for creating a hashtable
    where the first argument is the initial size of the table, and the
    second argument is a flag which, when true, indicates that the
    hashtable will reduce its size when the hashtable contains few
    elements.  The following signature doesn't give you much of a hint
    as to the meaning of the arguments.

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
    val create_hashtable : int -> bool -> ('a,'b) Hashtable.t
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    but with labeled arguments, we can make the intent much clearer.

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
    val create_hashtable : init_size:int -> allow_shrinking:bool -> ('a,'b) Hashtable.t
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

  - When you want flexibility on the order in which arguments are
    presented and the order of partial application.  One common
    example is functions like `List.map` or List.fold which take a
    function as one of their arguments.  When the function in question
    is big, it's often more readable to put the function last, _e.g._:

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
    # let rot13 s =
         String.map s ~f:(fun c ->
            if not (Char.is_alpha c) then c
            else
              let a_int = Char.to_int 'a' in
              let offset = Char.to_int (Char.lowercase c) - a_int in
              let c' = Char.of_int_exn ((offset + 13) mod 26 + a_int) in
              if Char.is_uppercase c then Char.uppercase c' else c'
          );;
    val rot13 : string -> string = <fun>
    # rot13 "Hello world!";;
    - : string = "Uryyb jbeyq!"
    # rot13 (rot13 "Hello world!");;
    - : string = "Hello world!"
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    But despite the fact that we often want the argument `f` to go
    last, we sometimes want to partially apply that argument.  In this
    example, we do so with `String.map`.

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
    # List.map ~f:(String.map ~f:Char.uppercase)
        [ "Hello"; "World" ];;
    - : string list = ["HELLO"; "WORLD"]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Higher-order functions and labels ####

One surprising gotcha labeled arguments is that while order doesn't
matter when calling a function with labeled arguments, it does matter
in a higher-order context, _e.g._, when passing a function with
labeled arguments to another function.  Here's an example.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let apply_to_tuple f (first,second) = f ~first ~second;;
val apply_to_tuple : (first:'a -> second:'b -> 'c) -> 'a * 'b -> 'c = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here, the definition of `apply_to_tuple` sets up the expectation that
its first argument is a function with two labeled arguments, `first`
and `second`, listed in that order.  We could have defined
`apply_to_tuple` differently to change the order in which the labeled
arguments were listed.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let apply_to_tuple f (first,second) = f ~second ~first;;
val apply_to_tuple : (second:'a -> first:'b -> 'c) -> 'b * 'a -> 'c = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

It turns out this order of listing matters.  In particular, if we
define a function that has a different order

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let divide ~first ~second = first / second;;
val divide : first:int -> second:int -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

we'll find that it can't be passed in to `apply_to_tuple`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# apply_to_tuple divide (3,4);;
Characters 15-21:
  apply_to_tuple divide (3,4);;
                 ^^^^^^
Error: This expression has type first:int -> second:int -> int
       but an expression was expected of type second:'a -> first:'b -> 'c
~~~~~~~~~~~~~~~~~~~~~~~~~~~

But, if we go back to the original definition of `apply_to_tuple`,
things will work smoothly.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let apply_to_tuple f (first,second) = f ~first ~second;;
val apply_to_tuple : (first:'a -> second:'b -> 'c) -> 'a * 'b -> 'c = <fun>
# apply_to_tuple divide (3,4);;
- : int = 0
~~~~~~~~~~~~~~~~~~~~~~~~~~~

So, even though the order of labeled arguments usually doesn't matter,
it will sometimes bite you in higher-ordered contexts, where you're
doing things like passing functions as arguments to other functions.

### Optional arguments ###

An optional argument is like a labeled argument that the caller can
choose whether or not to provide.  Optional arguments are passed in
using the same syntax as labeled arguments, and, similarly to labeled
arguments, optional arguments can be provided in any order.

Here's an example of a string concatenation function with an optional
separator.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let concat ?sep x y =
     let sep = match sep with None -> "" | Some x -> x in
     x ^ sep ^ y
  ;;
val concat : ?sep:string -> string -> string -> string = <fun>
# concat "foo" "bar";;             (* without the optional argument *)
- : string = "foobar"
# concat ~sep:":" "foo" "bar";;    (* with the optional argument    *)
- : string = "foo:bar"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here, `?` is used to mark the separator as optional.  Note that, while
the type of the optional argument is `string`, internally, the
argument is received as a `string option`, where `None` indicates that
the optional argument was not specified.

In the above example, we had a bit of code to substitute in the empty
string when no argument was provided.  This is a common enough pattern
that there's an explicit syntax for doing this, which allows us to
write `concat` even more tersely:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let concat ?(sep="") x y = x ^ sep ^ y ;;
val concat : ?sep:string -> string -> string -> string = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Optional arguments are very useful, but they're also easy to abuse.
The key advantage of optional arguments is that they let you write
functions with complex options that users can ignore most of the time,
only needing to think about them when they specifically want to invoke
those options.

The downside is that it's easy for the caller of a function to not be
aware that there is a choice to be made, leading them to pick the
default behavior unknowingly, and sometimes wrongly.  Optional
arguments really only make sense when the extra concision of omitting
the argument overwhelms the corresponding loss of explicitness.

This means that rarely used functions should not have optional
arguments.  A good rule of thumb for optional arguments is that you
should never use an optional argument for internal functions of a
module, only for functions that are exposed to users of a module.


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
# uppercase_concat "foo" "bar" ~sep:":";;
- : string = "FOO:bar"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Inference of labeled and optional arguments

_(yminsky: This is too abstract of an example.)_

One subtle aspect of labeled and optional arguments is how they are
inferred by the type system.  Consider the following example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let foo g x y = g ~x ~y ;;
val foo : (x:'a -> y:'b -> 'c) -> 'a -> 'b -> 'c = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In principle, it seems like the inferred type of `g` could have its
labeled arguments listed in a different order, such as:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
val foo : (y:'b -> x:'a -> 'c) -> 'a -> 'b -> 'c = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

And it would be perfectly consistent for `g` to take an optional
argument instead of a labeled one, which could lead to this type
signature for `foo`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
val foo : (?x:'a -> y:'b -> 'c) -> 'a -> 'b -> 'c = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Since there are multiple plausible types to choose from, OCaml needs
some heuristic for choosing between them.  The heuristic the compiler
uses is to prefer labels to options, and to choose the order of
arguments that shows up in the source code.

Note that these heuristics might at different points in the source
suggest different types.  For example, here's a function whose
argument `g` is a function that is used once with argument `~x`
followed by `~y`, and once with argument `~y` followed by `~x`.  The
result of this is a compilation error.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let bar g x y = g ~x ~y + g ~y ~x ;;
Characters 26-27:
  let bar g x y = g ~x ~y + g ~y ~x ;;
                            ^
Error: This function is applied to arguments
in an order different from other calls.
This is only allowed when the real type is known.
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that if we provide an explicit type constraint for `g`, that
constraint decides the question of what `g`'s type is, and the error
disappears.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml }
# let foo (g : ?y:'a -> x:'b -> int) x y =
    g ~x ~y + g ~y ~x ;;
val foo : (?y:'a -> x:'b -> int) -> 'b -> 'a -> int = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Optional arguments and partial application ###

Optional arguments can be tricky to think about in the presence of
partial application.  We can of course partially apply the optional
argument itself:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let colon_concat = concat ~sep:":";;
val colon_concat : string -> string -> string = <fun>
# colon_concat "a" "b";;
- : string = "a:b"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

But what happens if we partially apply just the first argument?

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let prepend_pound = concat "# ";;
val prepend_pound : string -> string = <fun>
# prepend_pound "a BASH comment";;
- : string = "# a BASH comment"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that the optional argument `?sep` has now disappeared, or
_erased_.  So when does OCaml decide to erase an optional argument?

The rule is: an optional argument is erased as soon as the first
positional argument defined _after_ the optional argument is passed
in.  That explains the behavior of `prepend_pound` above.  But if we
had instead defined `concat` with the optional argument in the second
position:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let concat x ?(sep="") y = x ^ sep ^ y ;;
val concat : string -> ?sep:string -> string -> string = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~

then application of the first argument would not cause the optional
argument to be erased.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let prepend_pound = concat "# ";;
val prepend_pound : ?sep:string -> string -> string = <fun>
# prepend_pound "a BASH comment";;
- : string = "# a BASH comment"
# prepend_pound "a BASH comment" ~sep:"--- ";;
- : string = "# --- a BASH comment"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

However, if all arguments to a function are presented at once, then
erasure of optional arguments isn't applied until all of the arguments
are passed in.  This preserves our ability to pass in optional
arguments anywhere on the argument list.  Thus, we can write:

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# concat "a" "b" ~sep:"=";;
- : string = "a=b"
~~~~~~~~~~~~~~~~~~~~~~~~~~~

An optional argument that doesn't have any following positional
arguments can't be erased at all, which leads to a compiler warning.

~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .ocaml-toplevel }
# let concat x y ?(sep="") = x ^ sep ^ y ;;
Characters 15-38:
  let concat x y ?(sep="") = x ^ sep ^ y ;;
                 ^^^^^^^^^^^^^^^^^^^^^^^
Warning 16: this optional argument cannot be erased.
val concat : string -> string -> ?sep:string -> string = <fun>
~~~~~~~~~~~~~~~~~~~~~~~~~~~
