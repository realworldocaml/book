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

```ocaml
let <identifier> = <expr>
```

As we'll see when we get to the module system in
[xref](#files-modules-and-programs), this same syntax is used for
top-level definitions in a module.

Every variable binding has a _scope_, which is the portion of the code
that can refer to that binding.  The scope of a top-level let binding
is everything that follows it in the top-level session (or in the
remainder of the module).

Here's a simple example.

```ocaml
# let x = 3;;
val x : int = 3
# let y = 4;;
val y : int = 4
# let z = x + y;;
val z : int = 7
```

`let` can also be used to create a variable binding whose scope is
limited to a particular expression, using the following syntax.

```ocaml
let <identifier> = <expr1> in <expr2>
```

This first evaluates _`expr1`_ and then evaluates _`expr2`_ with
_`identifier`_ bound to whatever value was produced by the evaluation
of _`expr1`_.  Here's how it looks in practice.

```ocaml
# let languages = "OCaml,Perl,C++,C";;
val languages : string = "OCaml,Perl,C++,C"
# let dashed_languages =
    let language_list = String.split languages ~on:',' in
    String.concat ~sep:"-" language_list
  ;;
val dashed_languages : string = "OCaml-Perl-C++-C"
```

Note that the scope of `language_list` is just the expression
`String.concat ~sep:"-" language_list`, and is not available at the
top-level, as we can see if we try to access it now.

```ocaml
# language_list;;
Characters 0-13:
  language_list;;
  ^^^^^^^^^^^^^
Error: Unbound value language_list
```

A let binding in an inner scope can _shadow_, or hide, the definition
from an outer scope.  So, for example, we could have written the
`dashed_languages` example as follows:

```ocaml
# let languages = "OCaml,Perl,C++,C";;
val languages : string = "OCaml,Perl,C++,C"
# let dashed_languages =
     let languages = String.split languages ~on:',' in
     String.concat ~sep:"-" languages
  ;;
val dashed_languages : Core.Std.String.t = "OCaml-Perl-C++-C"
```

This time, in the inner scope we called the list of strings
`languages` instead of `language_list`, thus hiding the original
definition of `languages`.  But once the definition of
`dashed_languages` is complete, the inner scope has closed and the
original definition of languages reappears.

```ocaml
# languages;;
- : string = "OCaml,Perl,C++,C"
```

One common idiom is to use a series of nested `let`/`in` expressions
to build up the components of a larger computation.  Thus, we might
write:

```ocaml
# let area_of_ring inner_radius outer_radius =
     let pi = acos (-1.) in
     let area_of_circle r = pi *. r *. r in
     area_of_circle outer_radius -. area_of_circle inner_radius
  ;;
# area_of_ring 1. 3.;;
- : float = 25.1327412287183449
```

It's important not to confuse this sequence of let bindings with the
modification of a mutable variable.  How would `area_of_ring` be
different, for example, if we had instead written this purposefully
confusing bit of code:

```ocaml
# let area_of_ring inner_radius outer_radius =
     let pi = acos (-1.) in
     let area_of_circle r = pi *. r *. r in
     let pi = 0. in
     area_of_circle outer_radius -. area_of_circle inner_radius
  ;;
```

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

```ocaml
Characters 126-128:
    let pi = 0. in
        ^^
Warning 26: unused variable pi.
```

In OCaml, let bindings are immutable.  As we'll see in chapter
{{{imperative-programming}}}, there are mutable values in OCaml, but
no mutable variables.

### Pattern matching and `let` ###

Another useful feature of let bindings is that they support the use of
patterns on the left-hand side of the bind.  Consider the following
code, which uses `List.unzip`, a function for converting a list of
pairs into a pair of lists.

```ocaml
# let (ints,strings) = List.unzip [(1,"one"); (2,"two"); (3,"three")]
val ints : int list = [1; 2; 3]
val strings : string list = ["one"; "two"; "three"]
```

This actually binds two variables, one for each element of the pair.
Using a pattern in a let-binding makes the most sense for a pattern
that is _irrefutable_, _i.e._, where any value of the type in question
is guaranteed to match the pattern.  Tuple and record patterns are
irrefutable, but list patterns are not.  Consider the following code
that implements a function for up-casing the first element of a
comma-separate list.

```ocaml
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
```

This case can't really come up in practice, because `String.split`
always returns a list with at least one element.  But the compiler
doesn't know this, and so it emits the warning.  It's generally better
to use a match statement to handle such cases explicitly:

```ocaml
# let upcase_first_entry line =
     match String.split ~on:',' line with
     | [] -> assert false (* String.split returns at least one element *)
     | first :: rest -> String.concat ~sep:"," (String.uppercase first :: rest)
  ;;
val upcase_first_entry : string -> string = <fun>
```

### `let`/`and` bindings ###

Another variant on the let binding is the use of `and` to join
multiple variable definitions into a single declaration.  For example,
we can write:

```ocaml
# let x = 100 and y = 3.5;;
val x : int = 100
val y : float = 3.5
```

This can be useful when you want to create a number of new let
bindings at once, without having each definition affect the next.  So,
if we wanted to create new bindings that swapped the values of `x` and
`y`, we could write:

```ocaml
# let x = y and y = x ;;
val x : float = 3.5
val y : int = 100
```

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

```ocaml
# (fun x -> x + 1);;
- : int -> int = <fun>
```

Anonymous functions aren't named, but they can be used for many
different purposes nonetheless.  You can, for example, apply an
anonymous function to an argument:

```ocaml
# (fun x -> x + 1) 7;;
- : int = 8
```

Or pass it to another function.

```ocaml
# List.map ~f:(fun x -> x + 1) [1;2;3];;
- : int list = [2; 3; 4]
```

Or even stuff them into a datastructure.

```ocaml
# let increments = [ (fun x -> x + 1); (fun x -> x + 2) ] ;;
val increments : (int -> int) list = [<fun>; <fun>]
# List.map ~f:(fun f -> f 5) increments;;
- : int list = [6; 7]
```

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

```ocaml
# let plusone = (fun x -> x + 1);;
val plusone : int -> int = <fun>
# plusone 3;;
- : int = 4
```

Defining named functions is so common that there is some built in
syntactic sugar for it.  Thus, we can write:

```ocaml
# let plusone x = x + 1;;
val plusone : int -> int = <fun>
```

This is the most common and convenient way to declare a function, but
syntactic niceties aside, the two styles of function definition are
entirely equivalent.

<sidebar>
<title>`let` and `fun`</title>

Functions and let bindings have a lot to do with each other.  In some
sense, you can think of the argument of a function as a variable being
bound to its argument.  Indeed, the following two expressions are
nearly equivalent:

```ocaml
# (fun x -> x + 1) 7;;
- : int = 8
# let x = 7 in x + 1;;
- : int = 8
```

This connection is important, and will come up more when programming
in a monadic style, as we'll see in chapter {{ASYNC}}.

</sidebar>

### Multi-argument functions ###

OCaml of course also supports multi-argument functions.  Here's an
example that came up in chapter {{TOUR}}.

```ocaml
# let abs_diff x y = abs (x - y);;
val abs_diff : int -> int -> int = <fun>
# abs_diff 3 4;;
- : int = 1
```

You may find the type signature of `abs_diff` with all of its arrows a
little hard to parse.  To understand what's going on, let's rewrite
`abs_diff` in an equivalent form, using the `fun` keyword:

```ocaml
# let abs_diff =
    (fun x -> (fun y -> abs (x - y)));;
val abs_diff : int -> int -> int = <fun>
```

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

```ocaml
val abs_diff : int -> (int -> int)
```

Currying is more than just a theoretical curiosity.  You can make use
of currying to specialize a function by feeding in some of the
arguments.  Here's an example where we create a specialized version of
`abs_diff` that measures the distance of a given number from `3`.

```ocaml
# let dist_from_3 = abs_diff 3;;
val dist_from_3 : int -> int = <fun>
# dist_from_3 8;;
- : int = 5
# dist_from_3 (-1);;
- : int = 4
```

The practice of applying some of the arguments of a curried function
to get a new function is called _partial application_.

Note that the `fun` keyword supports its own syntactic sugar for
currying, so we could also have written `abs_diff` as follows.

```ocaml
# let abs_diff = (fun x y -> abs (x - y));;
```

You might worry that curried functions are terribly expensive, but
this is not the case.  In OCaml, there is no penalty for calling a
curried function with all of its arguments.  (Partial application,
unsurprisingly, does have a small extra cost.)

Currying is not the only way of writing a multi-argument function in
OCaml.  It's also possible to use the different arms of a tuple as
different arguments.  So, we could write:

```ocaml
# let abs_diff (x,y) = abs (x - y)
val abs_diff : int * int -> int = <fun>
# abs_diff (3,4);;
- : int = 1
```

OCaml handles this calling convention efficiently as well.  In
particular it does not generally have to allocate a tuple just for the
purpose of sending arguments to a tuple-style function.

There are small tradeoffs between these two approaches, but most of
the time, one should stick to currying, since it's the default style
in the OCaml world.

### Recursive functions ###

In order to define a recursive function, you need to mark the let
binding as recursive with the `rec` keyword, as shown in this example:

```ocaml
# let rec find_first_stutter = function
     | [] | [_] ->
       (* only zero or one elements, so no repeats *)
       None
     | x :: y :: tl ->
       if x = y then Some x else find_first_stutter (y::tl)
val find_first_stutter : 'a list -> 'a option = <fun>
```

We can also define multiple mutually recursive values by using `let
rec` and `and` together, as in this (gratuitously inefficient)
example.

```ocaml
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
```

Note that in the above example, we take advantage of the fact that the
right hand side of `||` is evaluated lazily, only being executed if
the left hand side evaluates to false.

### Prefix and Infix operators ###

So far, we've seen examples of functions used in both prefix and infix
style:

```ocaml
# Int.max 3 4;;  (* prefix *)
- : int = 4
# 3 + 4;;        (* infix  *)
- : int = 7
```

You might not have thought of the second example as an ordinary
function, but it very much is.  Infix operators like `+` really only
differ syntactically from other functions.  In fact, if we put
parenthesis around an infix operator, you can use it as an ordinary
prefix function.

```ocaml
# (+) 3 4;;
- : int = 7
# List.map ~f:((+) 3) [4;5;6];;
- : int list = [7; 8; 9]
```

In the second expression above, we've partially applied `(+)` to gain
a function that increments its single argument by `3`, and then
applied that to all the elements of a list.

A function is treated syntactically as an operator if the name of that
function is chosen from one of a specialized set of identifiers.  This
set includes any identifier that is a sequence of characters from the
following set

```
! $ % & * + - . / : < = > ? @ ^ | ~
```

or is one of a handful of pre-determined strings, including `mod`, the
modulus operator, and `lsl`, for "logical shift left", a bit-shifting
operation.

We can define (or redefine) the meaning of an operator as follows.
Here's an example of a simple vector-addition operator on int pairs.

```ocaml
# let (+!) (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)
val ( +! ) : int * int -> int * int -> int *int = <fun>
# (3,2) +! (-2,4);;
- : int * int = (1,6)
```

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

```ocaml
# let (|!) x f = f x ;;
val ( |! ) : 'a -> ('a -> 'b) -> 'b = <fun>
```

It's not quite obvious at first what the purpose of this operator is:
it just takes some value and a function, and applies the function to
the value.  But its utility is clearer when you see it in action.  It
works as a kind of sequencing operator, similar in spirit to using
pipe in the UNIX shell.  Consider, for example, the following code for
printing out the unique elements of your `PATH`.

```ocaml
# Sys.getenv_exn "PATH"
  |! String.split ~on:':'
  |! List.dedup ~compare:String.compare
  |! List.iter ~f:print_endline
  ;;
/bin
/opt/local/bin
/usr/bin
/usr/local/bin
- : unit = ()
```

An important part of what's happening here is partial application.
Normally, `List.iter` takes two arguments: a function to be called on
each element of the list, and the list to iterate over.  We can call
`List.iter` with all it's arguments:

```ocaml
# List.iter ~f:print_endline ["Two"; "lines"];;
Two
lines
- : unit = ()
```

Or, we can pass it just the function argument, leaving us with a
function for printing out a list of strings.

```ocaml
# List.iter ~f:print_endline;;
- : string list -> unit = <fun>
```

It is this later form that we're using in the `|!` pipeline above.

Note that `|!` only works in the intended way because it is
left-associative.  Indeed, let's see what happens if we try using a
right associative operator, like (^!).

```ocaml
# let (^!) = (|!);;
val ( ^! ) : 'a -> ('a -> 'b) -> 'b = <fun>
# Sys.getenv_exn "PATH"
  ^! String.split ~on:':'
  ^! List.dedup ~compare:String.compare
  ^! List.iter ~f:print_endline
  ;;
        Characters 93-119:
    ^! List.iter ~f:print_endline
       ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type string list -> unit
       but an expression was expected of type
         (string list -> string list) -> 'a
```

The above type error is a little bewildering at first glance.  What's
going on is that, because `^!` is right associative, the operator is
trying to feed the value `List.dedup ~compare:String.compare` to the
function `List.iter ~f:print_endline`.  But `List.iter
~f:print_endline` expects a list of strings as its input, not a
function.

The type error aside, this example highlights the importance of
choosing the operator you use with care, particularly with respect to
associativity.

### Declaring functions with `function` ###

Another way to define a function is using the `function` keyword.
Instead of having syntactic support for declaring curried functions,
`function` has built-in pattern matching.  Here's an example:

```ocaml
# let some_or_zero = function
     | Some x -> x
     | None -> 0
  ;;
val some_or_zero : int option -> int = <fun>
# List.map ~f:some_or_zero [Some 3; None; Some 4];;
- : int list = [3; 0; 4]
```

This is equivalent to combining a `fun` with `match`, as follows:

```ocaml
# let some_or_zero num_opt =
    match num_opt with
    | Some x -> x
    | None -> 0
  ;;
val some_or_zero : int option -> int = <fun>
```

We can also combine the different styles of function declaration
together, as in the following example where we declare a two argument
function with a pattern-match on the second argument.

```ocaml
# let some_or_default default = function
     | Some x -> x
     | None -> default
  ;;
# List.map ~f:(some_or_default 100) [Some 3; None; Some 4];;
- : int Core.Std.List.t = [3; 100; 4]
```

Also, note the use of partial application to generate the function
passed to `List.map`

### Labeled Arguments ###

Up until now, we've written functions where the arguments are
specified positionally, _i.e._, by the order in which the arguments
are passed to the function.  OCaml also supports labeled arguments,
which let you identify a function argument by name.  Labeled arguments
are declared by putting a tilde in front of the label.

```ocaml
# let f ~foo:a ~bar:b = a + b;;
val f : foo:int -> bar:int -> int = <fun>
```

We can then provide a labeled argument using a similar convention.  As
you can see, the arguments can be provided in any order.

```ocaml
# f ~foo:3 ~bar:10;;
- : int = 13
# f ~bar:10 ~foo:3;;
- : int = 13
```

OCaml also supports _label punning_, meaning that you get to drop the
text after the `:` if the name of the label and the name of the
variable being used are the same.  Label punning works in both
function declaration and function invocation, as shown in these
examples:

```ocaml
# let foo = 3;;
# let bar = 4;;
# f ~foo ~bar;;
- : int = 7
```

Labeled arguments are useful in a few different cases:

  - When defining a function with lots of arguments.  Beyond a certain
    number, arguments are easier to remember by name than by position.

  - When defining functions that have multiple arguments that might
    get confused with each other.  This is most at issue when the
    arguments are of the same type.  For example, consider this
    signature for a function for extracting a substring of another
    string.

    ```ocaml
    val substring: string -> int -> int -> string
    ```

    where the two ints are the starting position and length of the
    substring to extract.  Labeled arguments can make this signature
    clearer:

    ```ocaml
    val substring: string -> pos:int -> len:int -> string
    ```

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

    ```ocaml
    val create_hashtable : int -> bool -> ('a,'b) Hashtable.t
    ```

    but with labeled arguments, we can make the intent much clearer.

    ```ocaml
    val create_hashtable : init_size:int -> allow_shrinking:bool -> ('a,'b) Hashtable.t
    ```

  - When you want flexibility on the order in which arguments are
    passed.  Consider a function like `List.iter`, that takes two
    arguments: a function, and a list of elements to call that
    function on.  A common pattern is to partially apply `List.iter`
    by giving it just the function, as in the following example from
    earlier in the chapter.  This requires putting the function
    argument first.

    ```ocaml
    # Sys.getenv_exn "PATH"
      |! String.split ~on:':'
      |! List.dedup ~compare:String.compare
      |! List.iter ~f:print_endline
      ;;
    ```

    In other cases, you want to put the function argument second.  One
    common reason is readability.  In particular, a function that
    spans multiple lines is easiest to read when it's the last
    argument provided.

#### Higher-order functions and labels ####

One surprising gotcha with labeled arguments is that while order
doesn't matter when calling a function with labeled arguments, it does
matter in a higher-order context, _e.g._, when passing a function with
labeled arguments to another function.  Here's an example.

```ocaml
# let apply_to_tuple f (first,second) = f ~first ~second;;
val apply_to_tuple : (first:'a -> second:'b -> 'c) -> 'a * 'b -> 'c = <fun>
```

Here, the definition of `apply_to_tuple` sets up the expectation that
its first argument is a function with two labeled arguments, `first`
and `second`, listed in that order.  We could have defined
`apply_to_tuple` differently to change the order in which the labeled
arguments were listed.

```ocaml
# let apply_to_tuple f (first,second) = f ~second ~first;;
val apply_to_tuple : (second:'a -> first:'b -> 'c) -> 'b * 'a -> 'c = <fun>
```

It turns out this order of listing matters.  In particular, if we
define a function that has a different order

```ocaml
# let divide ~first ~second = first / second;;
val divide : first:int -> second:int -> int = <fun>
```

we'll find that it can't be passed in to `apply_to_tuple`.

```ocaml
# apply_to_tuple divide (3,4);;
Characters 15-21:
  apply_to_tuple divide (3,4);;
                 ^^^^^^
Error: This expression has type first:int -> second:int -> int
       but an expression was expected of type second:'a -> first:'b -> 'c
```

But, if we go back to the original definition of `apply_to_tuple`,
things will work smoothly.

```ocaml
# let apply_to_tuple f (first,second) = f ~first ~second;;
val apply_to_tuple : (first:'a -> second:'b -> 'c) -> 'a * 'b -> 'c = <fun>
# apply_to_tuple divide (3,4);;
- : int = 0
```

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

```ocaml
# let concat ?sep x y =
     let sep = match sep with None -> "" | Some x -> x in
     x ^ sep ^ y
  ;;
val concat : ?sep:string -> string -> string -> string = <fun>
# concat "foo" "bar";;             (* without the optional argument *)
- : string = "foobar"
# concat ~sep:":" "foo" "bar";;    (* with the optional argument    *)
- : string = "foo:bar"
```

Here, `?` is used to mark `sep` as optional.  And while the caller can
pass a value of type `string` for `sep`, internally to the function,
`sep` is seen as a `string option`, with `None` appearing when `sep`
is not provided by the caller.

In the above example, we had a bit of code to substitute in the empty
string when no argument was provided.  This is a common enough pattern
that there's an explicit syntax for providing a default value, which
allows us to write `concat` even more concisely.

```ocaml
# let concat ?(sep="") x y = x ^ sep ^ y ;;
val concat : ?sep:string -> string -> string -> string = <fun>
```

Optional arguments are very useful, but they're also easy to abuse.
The key advantage of optional arguments is that they let you write
functions with multiple arguments that users can ignore most of the
time, only worrying about them when they specifically want to invoke
those options.

The downside is that the caller may be unaware that there is a choice
to be made, and so may unknowingly (and wrongly) pick that default
behavior.  Optional arguments really only make sense when the extra
concision of omitting the argument overwhelms the corresponding loss
of explicitness.

This means that rarely used functions should not have optional
arguments.  A good rule of thumb for optional arguments is that you
should never use an optional argument for internal functions of a
module, only for functions that are exposed as part of the module's
interface.

#### Explicit passing of an optional argument ###

Under the covers, a function with an optional argument receives `None`
when the caller doesn't provide the argument, and `Some` when it does.
But the `Some` and `None` are normally not explicitly passed in by the
caller.

But sometimes, passing in `Some` or `None` explicitly is exactly what
you want.  OCaml lets you do this by using `?` instead of `~` to mark
the argument.  Thus, the following two lines are equivalent ways of
specifying the `sep` argument to concat.

```ocaml
# concat ~sep:":" "foo" "bar";; (* provide the optional argument *)
- : string = "foo:bar"
# concat ?sep:(Some ":") "foo" "bar";; (* pass an explicit [Some] *)
- : string = "foo:bar"
```

And the following two lines are equivalent ways of calling `concat`
without specifying `sep`.

```ocaml
# concat "foo" "bar";; (* don't provide the optional argument *)
- : string = "foobar"
# concat ?sep:None "foo" "bar";; (* explicitly pass `None` *)
- : string = "foobar"
```

One use-case for this is when you want to define a wrapper function
that mimics the optional arguments of the function it's wrapping.  For
example, imagine we wanted to create a function called
`uppercase_concat`, which is the same as `concat` except that it
converts the first string that it's passed to uppercase.  We could
write the function as follows.

```ocaml
# let uppercase_concat ?(sep="") a b = concat ~sep (String.uppercase a) b ;;
val uppercase_concat : ?sep:string -> string -> string -> string = <fun>
# uppercase_concat "foo" "bar";;
- : string = "FOObar"
# uppercase_concat "foo" "bar" ~sep:":";;
- : string = "FOO:bar"
```

In the way we've written it, we've been forced to separately make the
decision as to what the default separator is.  Thus, if we later
change `concat`'s default behavior, we'll need to remember to change
`uppercase_concat` to match it.

Instead, we can have `uppercase_concat` simply pass through the
optional argument to `concat` using the `?` syntax.

```ocaml
# let uppercase_concat ?sep a b = concat ?sep (String.uppercase a) b ;;
val uppercase_concat : ?sep:string -> string -> string -> string = <fun>
```

Now, if someone calls `uppercase_concat` without an argument, an
explicit `None` will be passed to `concat`, leaving `concat` to decide
what the default behavior should be.

#### Inference of labeled and optional arguments

_(yminsky: This is too abstract of an example.)_

One subtle aspect of labeled and optional arguments is how they are
inferred by the type system.  Consider the following example for
computing numerical derivatives of a function of two dimensions.  The
function takes an argument `delta` which determines the scale at which
to compute the derivative, values `x` and `y` which determine which
point to compute the derivative at, and the function `f` whose
derivative is being computed.  The function `f` itself takes two
labeled arguments `x` and `y`.

```ocaml
# let numeric_deriv ~delta ~x ~y ~f =
    let x' = x +. delta in
    let y' = y +. delta in
    let base = f ~x ~y in
    let dx = (f ~x:x' ~y -. base) /. delta in
    let dy = (f ~x ~y:y' -. base) /. delta in
    (dx,dy)
  ;;
val numeric_deriv :
  delta:float ->
  x:float -> y:float -> f:(x:float -> y:float -> float) -> float * float =
  <fun>
```

In principle, it's not obvious how the order of the arguments to `f`
should be chosen.  Since optional arguments can be passed in arbitrary
order, it seems like it could as well be `y:float -> x:float -> float`
as it is `x:float -> y:float -> float`.

Even worse, it would be perfectly consistent for `f` to take an
optional argument instead of a labeled one, which could lead to this
type signature for `numeric_deriv`:

```ocaml
val numeric_deriv :
  delta:float ->
  x:float -> y:float -> f:(?x:float -> y:float -> float) -> float * float =
  <fun>
```

Since there are multiple plausible types to choose from, OCaml needs
some heuristic for choosing between them.  The heuristic the compiler
uses is to prefer labels to options, and to choose the order of
arguments that shows up in the source code.

Note that these heuristics might at different points in the source
suggest different types.  Here's a version of `numeric_deriv` where
the invocations of `f` were changes so they list the arguments in
different orders.

```ocaml
# let numeric_deriv ~delta ~x ~y ~f =
    let x' = x +. delta in
    let y' = y +. delta in
    let base = f ~x ~y in
    let dx = (f ~y ~x:x' -. base) /. delta in
    let dy = (f ~x ~y:y' -. base) /. delta in
    (dx,dy)
  ;;
Characters 131-132:
      let dx = (f ~y ~x:x' -. base) /. delta in
                ^
Error: This function is applied to arguments
in an order different from other calls.
This is only allowed when the real type is known.
```

As suggested by the error message, we can get OCaml to accept the
fact that `f` is used with different argument orders if we provide
explicit type information.  Thus, we can write:

Note that if we provide an explicit type constraint for `g`, that
constraint decides the question of what `g`'s type is, and the error
disappears.  In the following, we do this by providing an explicit
type annotation.

```ocaml
# let numeric_deriv ~delta ~x ~y ~(f: x:float -> y:float -> float) =
    let x' = x +. delta in
    let y' = y +. delta in
    let base = f ~x ~y in
    let dx = (f ~y ~x:x' -. base) /. delta in
    let dy = (f ~x ~y:y' -. base) /. delta in
    (dx,dy)
  ;;
val numeric_deriv :
  delta:float ->
  x:float -> y:float -> f:(x:float -> y:float -> float) -> float * float =
  <fun>
```

#### Optional arguments and partial application ###

Optional arguments can be tricky to think about in the presence of
partial application.  We can of course partially apply the optional
argument itself:

```ocaml
# let colon_concat = concat ~sep:":";;
val colon_concat : string -> string -> string = <fun>
# colon_concat "a" "b";;
- : string = "a:b"
```

But what happens if we partially apply just the first argument?

```ocaml
# let prepend_pound = concat "# ";;
val prepend_pound : string -> string = <fun>
# prepend_pound "a BASH comment";;
- : string = "# a BASH comment"
```

Note that the optional argument `?sep` has now disappeared, or
_erased_.  So when does OCaml decide to erase an optional argument?

The rule is: an optional argument is erased as soon as the first
positional argument defined _after_ the optional argument is passed
in.  That explains the behavior of `prepend_pound` above.  But if we
had instead defined `concat` with the optional argument in the second
position:

```ocaml
# let concat x ?(sep="") y = x ^ sep ^ y ;;
val concat : string -> ?sep:string -> string -> string = <fun>
```

then application of the first argument would not cause the optional
argument to be erased.

```ocaml
# let prepend_pound = concat "# ";;
val prepend_pound : ?sep:string -> string -> string = <fun>
# prepend_pound "a BASH comment";;
- : string = "# a BASH comment"
# prepend_pound "a BASH comment" ~sep:"--- ";;
- : string = "# --- a BASH comment"
```

However, if all arguments to a function are presented at once, then
erasure of optional arguments isn't applied until all of the arguments
are passed in.  This preserves our ability to pass in optional
arguments anywhere on the argument list.  Thus, we can write:

```ocaml
# concat "a" "b" ~sep:"=";;
- : string = "a=b"
```

An optional argument that doesn't have any following positional
arguments can't be erased at all, which leads to a compiler warning.

```ocaml
# let concat x y ?(sep="") = x ^ sep ^ y ;;
Characters 15-38:
  let concat x y ?(sep="") = x ^ sep ^ y ;;
                 ^^^^^^^^^^^^^^^^^^^^^^^
Warning 16: this optional argument cannot be erased.
val concat : string -> string -> ?sep:string -> string = <fun>
```

And indeed, when we provide the two positions arguments, the `sep`
argument is not erased, instead returning a function that expects the
`sep` argument to be provided.

```ocaml
# concat "a" "b";;
- : ?sep:string -> string = <fun>
```
