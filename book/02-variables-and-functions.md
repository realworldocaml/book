# Variables and Functions {#variables-and-functions data-type=chapter}

Variables and functions are fundamental ideas that show up in virtually all
programming languages. OCaml has a different take on these concepts than most
languages you're likely to have encountered, so this chapter will cover
OCaml's approach to variables and functions in some detail, starting with the
basics of how to define a variable, and ending with the intricacies of
functions with labeled and optional arguments.

Don't be discouraged if you find yourself overwhelmed by some of the details,
especially toward the end of the chapter. The concepts here are important,
but if they don't connect for you on your first read, you should return to
this chapter after you've gotten a better sense of the rest of the language.

## Variables {#variables data-type=sect1}

At its simplest, a variable is an identifier whose meaning is bound to a
particular value. In OCaml these bindings are often introduced using the
`let` keyword. We can type a so-called *top-level*`let` binding with the
following syntax. Note that variable names must start with a lowercase letter
or an underscore. [bindings/top-level]{.idx}[top-level bindings]{.idx}[let
syntax/top-level bindings]{.idx}

<link rel="import" href="code/variables-and-functions/let.syntax" />

As we'll see when we get to the module system in
[Files Modules And Programs](04-files-modules-and-programs.html#files-modules-and-programs){data-type=xref},
this same syntax is used for `let` bindings at the top level of a module.

Every variable binding has a *scope*, which is the portion of the code that
can refer to that binding. When using `utop`, the scope of a top-level 
`let` binding is everything that follows it in the session. When it shows up
in a module, the scope is the remainder of that module.[variables/scope
of]{.idx}[bindings/scope of]{.idx}[scope]{.idx}

Here's a simple example.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"0.5" />

`let` can also be used to create a variable binding whose scope is limited to
a particular expression, using the following syntax.

<link rel="import" href="code/variables-and-functions/let_in.syntax" />

This first evaluates *`expr1`* and then evaluates *`expr2`* with *`variable`*
bound to whatever value was produced by the evaluation of *`expr1`*. Here's
how it looks in practice.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"1" />

Note that the scope of `language_list` is just the expression
`String.concat ~sep:"-" language_list` and is not available at the toplevel,
as we can see if we try to access it now.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"2" />

A `let` binding in an inner scope can *shadow*, or hide, the definition from
an outer scope. So, for example, we could have written the `dashed_languages`
example as follows. [variables/shadowing of]{.idx}[shadowing]{.idx}

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"3" />

This time, in the inner scope we called the list of strings `languages`
instead of `language_list`, thus hiding the original definition of
`languages`. But once the definition of `dashed_languages` is complete, the
inner scope has closed and the original definition of languages is still
available.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"4" />

One common idiom is to use a series of nested `let`/`in` expressions to build
up the components of a larger computation. Thus, we might write. [let
syntax/nested bindings]{.idx}[nested let binding]{.idx}

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"5" />

It's important not to confuse a sequence of `let` bindings with the
modification of a mutable variable. For example, consider how `area_of_ring`
would work if we had instead written this purposefully confusing bit of code:

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"6" />

Here, we redefined `pi` to be zero after the definition of `area_of_circle`.
You might think that this would mean that the result of the computation would
now be zero, but in fact, the behavior of the function is unchanged. That's
because the original definition of `pi` wasn't changed; it was just shadowed,
which means that any subsequent reference to `pi` would see the new
definition of `pi` as `0`, but earlier references would still see the old
one. But there is no later use of `pi`, so the binding of `pi` to `0.` made
no difference at all. This explains the warning produced by the toplevel
telling us that there is an unused variable.

In OCaml, `let` bindings are immutable. There are many kinds of mutable
values in OCaml, which we'll discuss in
[Imperative Programming](08-imperative-programming.html#imperative-programming-1){data-type=xref},
but there are no mutable variables.

::: {data-type=note}
### Why Don't Variables Vary?

One source of confusion for people new to OCaml is the fact that variables
are immutable. This seems pretty surprising even on linguistic terms. Isn't
the whole point of a variable that it can vary?[variables/immutability
of]{.idx}

The answer to this is that variables in OCaml (and generally in functional
languages) are really more like variables in an equation than a variable in
an imperative language. If you think about the mathematical identity
`x(y + z) = xy + xz`, there's no notion of mutating the variables `x`, 
`y`, and `z`. They vary in the sense that you can instantiate this equation
with different numbers for those variables, and it still holds.

The same is true in a functional language. A function can be applied to
different inputs, and thus its variables will take on different values, even
without mutation.
:::


### Pattern Matching and let {#pattern-matching-and-let data-type=sect2}

Another useful feature of `let` bindings is that they support the use of
*patterns* on the left-hand side. Consider the following code, which uses
`List.unzip`, a function for converting a list of pairs into a pair of
lists.[pattern matching/and let]{.idx}[let syntax/pattern
matching]{.idx}[variables/pattern matching in]{.idx}

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"7" />

Here, `(ints,strings)` is a pattern, and the `let` binding assigns values to
both of the identifiers that show up in that pattern. A pattern is
essentially a description of the shape of a data structure, where some
components are names to be bound to values. As we saw in
[Tuples Lists Options And Pattern Matching](01-guided-tour.html#tuples-lists-options-and-pattern-matching){data-type=xref},
OCaml has patterns for a variety of different data types.

Using a pattern in a `let` binding makes the most sense for a pattern that is
*irrefutable*, *i.e.*, where any value of the type in question is guaranteed
to match the pattern. Tuple and record patterns are irrefutable, but list
patterns are not. Consider the following code that implements a function for
upper casing the first element of a comma-separated list.[irrefutable
patterns]{.idx}

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"8" />

This case can't really come up in practice, because `String.split` always
returns a list with at least one element, even when given the empty string.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"8.1" />

But the compiler doesn't know this, and so it emits the warning. It's
generally better to use a `match` statement to handle such cases explicitly.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"9" />

Note that this is our first use of `assert`, which is useful for marking
cases that should be impossible. We'll discuss `assert` in more detail in
[Error Handling](07-error-handling.html#error-handling){data-type=xref}.


## Functions {#functions data-type=sect1}

Given that OCaml is a functional language, it's no surprise that functions
are important and pervasive. Indeed, functions have come up in almost every
example we've done so far. This section will go into more depth, explaining
the details of how OCaml's functions work. As you'll see, functions in OCaml
differ in a variety of ways from what you'll find in most mainstream
languages.

### Anonymous Functions {#anonymous-functions data-type=sect2}

We'll start by looking at the most basic style of function declaration in
OCaml: the *anonymous function*. An anonymous function is a function that is
declared without being named. These can be declared using the `fun` keyword,
as shown here. [fun keyword/anonymous functions]{.idx}[anonymous
functions]{.idx}[functions/anonymous functions]{.idx}

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"10" />

Anonymous functions operate in much the same way as named functions. For
example, we can apply an anonymous function to an argument.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"11" />

Or pass it to another function. Passing functions to iteration functions like
`List.map` is probably the most common use case for anonymous functions.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"12" />

You can even stuff a function into a data structure, like a list.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"13" />

It's worth stopping for a moment to puzzle this example out, since this kind
of higher-order use of functions can be a bit obscure at first. Notice that
`(fun g -> g "Hello World")` is a function that takes a function as an
argument, and then applies that function to the string `"Hello World"`. The
invocation of `List.map` applies `(fun g -> g "Hello World")` to the elements
of `transforms`, which are themselves functions. The returned list containing
the results of these function applications.

The key thing to understand is that functions are ordinary values in OCaml,
and you can do everything with them that you'd do with an ordinary value,
including passing them to and returning them from other functions and storing
them in data structures. We even name functions in the same way that we name
other values, by using a `let` binding.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"14" />

Defining named functions is so common that there is some syntactic sugar for
it. Thus, the following definition of `plusone` is equivalent to the previous
definition.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"15" />

This is the most common and convenient way to declare a function, but
syntactic niceties aside, the two styles of function definition are
equivalent.

::: {data-type=note}
#### let and fun

Functions and `let` bindings have a lot to do with each other. In some sense,
you can think of the parameter of a function as a variable being bound to the
value passed by the caller. Indeed, the following two expressions are nearly
equivalent. [let syntax/functions and]{.idx}

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"16" />

This connection is important, and will come up more when programming in a
monadic style, as we'll see in
[Concurrent Programming With Async](18-concurrent-programming.html#concurrent-programming-with-async){data-type=xref}.
:::


### Multiargument functions {#multi-argument-functions data-type=sect2}

OCaml of course also supports multiargument functions, such as:[fun
keyword/multi-argument functions]{.idx}[multi-argument
functions]{.idx}[functions/multi-argument functions]{.idx}

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"17" />

You may find the type signature of `abs_diff` with all of its arrows a little
hard to parse. To understand what's going on, let's rewrite `abs_diff` in an
equivalent form, using the `fun` keyword.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"18" />

This rewrite makes it explicit that `abs_diff` is actually a function of one
argument that returns another function of one argument, which itself returns
the final result. Because the functions are nested, the inner expression
`abs (x - y)` has access to both `x`, which was bound by the outer function
application, and `y`, which was bound by the inner one.

This style of function is called a *curried* function. (Currying is named
after Haskell Curry, a logician who had a significant impact on the design
and theory of programming languages.) The key to interpreting the type
signature of a curried function is the observation that `->` is
right-associative. The type signature of `abs_diff` can therefore be
parenthesized as follows. [curried functions]{.idx}[functions/curried
functions]{.idx}

<link rel="import" href="code/variables-and-functions/abs_diff.mli" />

The parentheses don't change the meaning of the signature, but they make it
easier to see the currying.

Currying is more than just a theoretical curiosity. You can make use of
currying to specialize a function by feeding in some of the arguments. Here's
an example where we create a specialized version of `abs_diff` that measures
the distance of a given number from `3`.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"19" />

The practice of applying some of the arguments of a curried function to get a
new function is called *partial application*.[partial application]{.idx}

Note that the `fun` keyword supports its own syntax for currying, so the
following definition of `abs_diff` is equivalent to the previous one.[fun
keyword/currying syntax]{.idx}

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"20" />

You might worry that curried functions are terribly expensive, but this is
not the case. In OCaml, there is no penalty for calling a curried function
with all of its arguments. (Partial application, unsurprisingly, does have a
small extra cost.)

Currying is not the only way of writing a multiargument function in OCaml.
It's also possible to use the different parts of a tuple as different
arguments. So, we could write.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"21" />

OCaml handles this calling convention efficiently as well. In particular it
does not generally have to allocate a tuple just for the purpose of sending
arguments to a tuple-style function. You can't, however, use partial
application for this style of function.

There are small trade-offs between these two approaches, but most of the
time, one should stick to currying, since it's the default style in the OCaml
world.

### Recursive Functions {#recursive-functions data-type=sect2}

A function is *recursive* if it refers to itself in its definition. Recursion
is important in any programming language, but is particularly important in
functional languages, because it is the way that you build looping
constructs. (As will be discussed in more detail in
[Imperative Programming 1](08-imperative-programming.html#imperative-programming-1){data-type=xref},
OCaml also supports imperative looping constructs like `for` and `while`, but
these are only useful when using OCaml's imperative features.)[recursive
functions/definition of]{.idx}[functions/recursive functions]{.idx}

In order to define a recursive function, you need to mark the `let` binding
as recursive with the `rec` keyword, as shown in this function for finding
the first sequentially repeated element in a list.[rec keyword]{.idx}

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"22" />

The pattern `[] | [_]` is itself a disjunction of multiple patterns,
otherwise known as an *or-pattern*. An or-pattern matches if any of the
sub-patterns match. In this case, `[]` matches the empty list, and `[_]`
matches any single element list. The `_` is there so we don't have to put an
explicit name on that single element.[or-patterns]{.idx}

We can also define multiple mutually recursive values by using `let rec`
combined with the `and` keyword. Here's a (gratuitously inefficient) example.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"23" />

OCaml distinguishes between nonrecursive definitions (using `let`) and
recursive definitions (using `let rec`) largely for technical reasons: the
type-inference algorithm needs to know when a set of function definitions are
mutually recursive, and these have to be marked explicitly by the programmer.
[let syntax/nonrecursive vs. recursive functions]{.idx}

But this decision has some good effects. For one thing, recursive (and
especially mutually recursive) definitions are harder to reason about than
nonrecursive ones. It's therefore useful that, in the absence of an explicit
`rec`, you can assume that a `let` binding is nonrecursive, and so can only
build upon previous definitions.

In addition, having a nonrecursive form makes it easier to create a new
definition that extends and supersedes an existing one by shadowing it.

### Prefix and Infix Operators {#prefix-and-infix-operators data-type=sect2}

So far, we've seen examples of functions used in both prefix and infix
style.[operators/prefix and infix operators]{.idx}[infix
operators]{.idx}[prefix operators]{.idx}[functions/prefix and infix
operators]{.idx #FNCprf}

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"24" />

You might not have thought of the second example as an ordinary function, but
it very much is. Infix operators like `+` really only differ syntactically
from other functions. In fact, if we put parentheses around an infix
operator, you can use it as an ordinary prefix function.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"25" />

In the second expression, we've partially applied `(+)` to create a function
that increments its single argument by `3`.

A function is treated syntactically as an operator if the name of that
function is chosen from one of a specialized set of identifiers. This set
includes identifiers that are sequences of characters from the following set:

<link rel="import" href="code/variables-and-functions/operators.syntax" />

or is one of a handful of predetermined strings, including `mod`, the modulus
operator, and `lsl`, for "logical shift left," a bit-shifting operation.

We can define (or redefine) the meaning of an operator. Here's an example of
a simple vector-addition operator on `int` pairs.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"26" />

Note that you have to be careful when dealing with operators containing 
`*`. Consider the following example.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"27" />

What's going on is that `(***)` isn't interpreted as an operator at all; it's
read as a comment! To get this to work properly, we need to put spaces around
any operator that begins or ends with `*`.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"28" />

The syntactic role of an operator is typically determined by its first
character or two, though there are a few exceptions.
[Table2_1](02-variables-and-functions.html#table2_1){data-type=xref} breaks
the different operators and other syntactic forms into groups from highest to
lowest precedence, explaining how each behaves syntactically. We write 
`!`... to indicate the class of operators beginning with `!`.

::: {#table2_1 data-type=table}
Operator prefix | Associativity
----------------|--------------
`!`..., `?`..., `~`... | Prefix
`.`, `.(`, `.[` | -
function application, constructor, `assert`, `lazy` | Left associative
`-`, `-.` | Prefix
`**`..., `lsl`, `lsr`, `asr` | Right associative
`*`..., `/`..., `%`..., `mod`, `land`, `lor`, `lxor` | Left associative
`+`..., `-`... | Left associative
`::` | Right associative
`@`..., `^`... | Right associative
`=`..., `<`..., `>`..., `|`..., `&`..., `$`... | Left associative
`&`, `&&` | Right associative
`or`, `||` | Right associative
`,` | -
`<-`, `:=` | Right associative
`if` | -
`;` | Right associative

Table:  Precedence and associativity 
:::



There's one important special case: `-` and `-.`, which are the integer and
floating-point subtraction operators, and can act as both prefix operators
(for negation) and infix operators (for subtraction). So, both `-x` and
`x - y` are meaningful expressions. Another thing to remember about negation
is that it has lower precedence than function application, which means that
if you want to pass a negative value, you need to wrap it in parentheses, as
you can see in this code.[operators/negation
operators]{.idx}[operators/subtraction operators]{.idx}[subtraction
operators]{.idx}[negation operators]{.idx}

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"29" />

Here, OCaml is interpreting the second expression as equivalent to.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"30" />

which obviously doesn't make sense.

Here's an example of a very useful operator from the standard library whose
behavior depends critically on the precedence rules described previously.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"31" />

It's not quite obvious at first what the purpose of this operator is: it just
takes a value and a function and applies the function to the value. Despite
that bland-sounding description, it has the useful role of a sequencing
operator, similar in spirit to using the pipe character in the UNIX shell.
Consider, for example, the following code for printing out the unique
elements of your `PATH`. Note that `List.dedup` that follows removes
duplicates from a list by sorting the list using the provided comparison
function.[lists/duplicate removal]{.idx}[duplicates,
removing]{.idx}[List.dedup]{.idx}[operators/sequencing operators]{.idx}

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"32" />

We can do this without `|>` by naming the intermediate values, but the result
is a bit more verbose.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"33" />

An important part of what's happening here is partial application. For
example, `List.iter` takes two arguments: a function to be called on each
element of the list, and the list to iterate over. We can call `List.iter`
with all its arguments. [partial application]{.idx}

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"34" />

Or, we can pass it just the function argument, leaving us with a function for
printing out a list of strings.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"35" />

It is this later form that we're using in the preceding `|>` pipeline.

But `|>` only works in the intended way because it is left-associative. Let's
see what happens if we try using a right-associative operator, like (^>).

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"36" />

The type error is a little bewildering at first glance. What's going on is
that, because `^>` is right associative, the operator is trying to feed the
value `List.dedup ~compare:String.compare` to the function
`List.iter ~f:print_endline`. But `List.iter ~f:print_endline` expects a list
of strings as its input, not a function.

The type error aside, this example highlights the importance of choosing the
operator you use with care, particularly with respect to
associativity.<a data-type="indexterm" data-startref="FNCprf">&nbsp;</a>

### Declaring Functions with `function` {#declaring-functions-with-function data-type=sect2}

Another way to define a function is using the `function` keyword. Instead of
having syntactic support for declaring multiargument (curried) functions,
`function` has built-in pattern matching. Here's an
example.[functions/defining]{.idx}[function
keyword]{.idx}[functions/declaring with function keyword]{.idx}

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"37" />

This is equivalent to combining an ordinary function definition with a
`match`.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"38" />

We can also combine the different styles of function declaration together, as
in the following example, where we declare a two-argument (curried) function
with a pattern match on the second argument.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"39" />

Also, note the use of partial application to generate the function passed to
`List.map`. In other words, `some_or_default 100` is a function that was
created by feeding just the first argument to `some_or_default`.

### Labeled Arguments {#labeled-arguments data-type=sect2}

Up until now, the functions we've defined have specified their arguments
positionally, *i.e.*, by the order in which the arguments are passed to the
function. OCaml also supports labeled arguments, which let you identify a
function argument by name. Indeed, we've already encountered functions from
`Base` like `List.map` that use labeled arguments. Labeled arguments are
marked by a leading tilde, and a label (followed by a colon) is put in front
of the variable to be labeled. Here's an example.[labeled
arguments]{.idx}[arguments/labeled arguments]{.idx}[functions/labeled
arguments]{.idx}

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"40" />

We can then provide a labeled argument using a similar convention. As you can
see, the arguments can be provided in any order.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"41" />

OCaml also supports *label punning*, meaning that you get to drop the text
after the `:` if the name of the label and the name of the variable being
used are the same. We were actually already using label punning when defining
`ratio`. The following shows how punning can be used when invoking a
function.[punning]{.idx}[label punning]{.idx}

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"42" />

Labeled arguments are useful in a few different cases:

- When defining a function with lots of arguments. Beyond a certain number,
  arguments are easier to remember by name than by position.[functions/with
  multiple arguments]{.idx}[multi-argument functions]{.idx}

- When the meaning of a particular argument is unclear from the type alone.
  Consider a function for creating a hash table whose first argument is the
  initial size of the array backing the hash table, and the second is a
  Boolean flag, which indicates whether that array will ever shrink when
  elements are removed.
  
  <link rel="import" href="code/variables-and-functions/htable_sig1.ml" />
  
  The signature makes it hard to divine the meaning of those two arguments.
  but with labeled arguments, we can make the intent immediately clear.
  
  <link rel="import" href="code/variables-and-functions/htable_sig2.ml" />
  
  Choosing label names well is especially important for Boolean values, since
  it's often easy to get confused about whether a value being true is meant
  to enable or disable a given feature.

- When defining functions that have multiple arguments that might get
  confused with each other. This is most at issue when the arguments are of
  the same type. For example, consider this signature for a function that
  extracts a substring.
  
  <link rel="import" href="code/variables-and-functions/substring_sig1.ml" />
  
  Here, the two `ints` are the starting position and length of the substring
  to extract, respectively, but you wouldn't know that from the type
  signature. We can make the signature more informative by adding labels.
  
  <link rel="import" href="code/variables-and-functions/substring_sig2.ml" />
  
  This improves the readability of both the signature and of client code that
  makes use of `substring` and makes it harder to accidentally swap the
  position and the length.

- When you want flexibility on the order in which arguments are passed.
  Consider a function like `List.iter`, which takes two arguments. a function
  and a list of elements to call that function on. A common pattern is to
  partially apply `List.iter` by giving it just the function, as in the
  following example from earlier in the chapter.
  
  <link rel="import" href="code/variables-and-functions/main.mlt" part=
  "43" />
  
  This requires that we put the function argument first. In other cases, you
  want to put the function argument second. One common reason is readability.
  In particular, a multiline function passed as an argument to another
  function is easiest to read when it is the final argument to that function.

#### Higher-order functions and labels {#higher-order-functions-and-labels data-type=sect3}

One surprising gotcha with labeled arguments is that while order doesn't
matter when calling a function with labeled arguments, it does matter in a
higher-order context, *e.g.*, when passing a function with labeled arguments
to another function. Here's an example.[higher-order functions, and
labels]{.idx}[functions/higher-order and labels]{.idx}

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"44" />

Here, the definition of `apply_to_tuple` sets up the expectation that its
first argument is a function with two labeled arguments, `first` and
`second`, listed in that order. We could have defined `apply_to_tuple`
differently to change the order in which the labeled arguments were listed.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"45" />

It turns out this order matters. In particular, if we define a function that
has a different order

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"46" />

we'll find that it can't be passed in to `apply_to_tuple_2`.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"47" />

But, it works smoothly with the original `apply_to_tuple`.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"48" />

As a result, when passing labeled functions as arguments, you need to take
care to be consistent in your ordering of labeled arguments.


### Optional Arguments {#optional-arguments data-type=sect2}

An optional argument is like a labeled argument that the caller can choose
whether or not to provide. Optional arguments are passed in using the same
syntax as labeled arguments, and, like labeled arguments, can be provided in
any order.[arguments/optional arguments]{.idx #ARGopt}[functions/optional
arguments]{.idx #FNCopt}

Here's an example of a string concatenation function with an optional
separator. This function uses the `^` operator for pairwise string
concatenation.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"49" />

Here, `?` is used in the definition of the function to mark `sep` as
optional. And while the caller can pass a value of type `string` for 
`sep`, internally to the function, `sep` is seen as a `string option`, with
`None` appearing when `sep` is not provided by the caller.

The preceding example needed a bit of boilerplate to choose a default
separator when none was provided. This is a common enough pattern that
there's an explicit syntax for providing a default value, which allows us to
write `concat` more concisely.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"50" />

Optional arguments are very useful, but they're also easy to abuse. The key
advantage of optional arguments is that they let you write functions with
multiple arguments that users can ignore most of the time, only worrying
about them when they specifically want to invoke those options. They also
allow you to extend an API with new functionality without changing existing
code.

The downside is that the caller may be unaware that there is a choice to be
made, and so may unknowingly (and wrongly) pick the default behavior.
Optional arguments really only make sense when the extra concision of
omitting the argument outweighs the corresponding loss of explicitness.

This means that rarely used functions should not have optional arguments. A
good rule of thumb is to avoid optional arguments for functions internal to a
module, *i.e.*, functions that are not included in the module's interface, or
`mli` file. We'll learn more about `mli`s in
[Files Modules And Programs](04-files-modules-and-programs.html#files-modules-and-programs){data-type=xref}.

#### Explicit passing of an optional argument {#explicit-passing-of-an-optional-argument data-type=sect3}

Under the covers, a function with an optional argument receives `None` when
the caller doesn't provide the argument, and `Some` when it does. But the
`Some` and `None` are normally not explicitly passed in by the caller.

But sometimes, passing in `Some` or `None` explicitly is exactly what you
want. OCaml lets you do this by using `?` instead of `~` to mark the
argument. Thus, the following two lines are equivalent ways of specifying the
`sep` argument to `concat`.[optional arguments/explicit passing of]{.idx}

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"51" />

And the following two lines are equivalent ways of calling `concat` without
specifying `sep`.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"52" />

One use case for this is when you want to define a wrapper function that
mimics the optional arguments of the function it's wrapping. For example,
imagine we wanted to create a function called `uppercase_concat`, which is
the same as `concat` except that it converts the first string that it's
passed to uppercase. We could write the function as follows.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"53" />

In the way we've written it, we've been forced to separately make the
decision as to what the default separator is. Thus, if we later change
`concat`'s default behavior, we'll need to remember to change
`uppercase_concat` to match it.

Instead, we can have `uppercase_concat` simply pass through the optional
argument to `concat` using the `?` syntax.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"54" />

Now, if someone calls `uppercase_concat` without an argument, an explicit
`None` will be passed to `concat`, leaving `concat` to decide what the
default behavior should be.

#### Inference of labeled and optional arguments {#inference-of-labeled-and-optional-arguments data-type=sect3}

One subtle aspect of labeled and optional arguments is how they are inferred
by the type system. Consider the following example for computing numerical
derivatives of a function of two real variables. The function takes an
argument `delta`, which determines the scale at which to compute the
derivative; values `x` and `y`, which determine at which point to compute the
derivative; and the function `f`, whose derivative is being computed. The
function `f` itself takes two labeled arguments, `x` and `y`. Note that you
can use an apostrophe as part of a variable name, so `x'` and `y'` are just
ordinary variables.[functions/argument inference]{.idx}[labeled
arguments]{.idx}[arguments/inference of]{.idx}[optional arguments/inference
of]{.idx}

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"55" />

In principle, it's not obvious how the order of the arguments to `f` should
be chosen. Since labeled arguments can be passed in arbitrary order, it seems
like it could as well be `y:float -> x:float -> float` as it is
`x:float -> y:float -> float`.

Even worse, it would be perfectly consistent for `f` to take an optional
argument instead of a labeled one, which could lead to this type signature
for `numeric_deriv`.

<link rel="import" href="code/variables-and-functions/numerical_deriv_alt_sig.mli" />

Since there are multiple plausible types to choose from, OCaml needs some
heuristic for choosing between them. The heuristic the compiler uses is to
prefer labels to options and to choose the order of arguments that shows up
in the source code.

Note that these heuristics might at different points in the source suggest
different types. Here's a version of `numeric_deriv` where different
invocations of `f` list the arguments in different orders.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"56" />

As suggested by the error message, we can get OCaml to accept the fact that
`f` is used with different argument orders if we provide explicit type
information. Thus, the following code compiles without error, due to the type
annotation on `f`.[type annotations]{.idx}

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"57" />

#### Optional arguments and partial application {#optional-arguments-and-partial-application data-type=sect3}

Optional arguments can be tricky to think about in the presence of partial
application. We can of course partially apply the optional argument itself.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"58" />

But what happens if we partially apply just the first argument?

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"59" />

The optional argument `?sep` has now disappeared, or been *erased*. Indeed,
if we try to pass in that optional argument now, it will be rejected.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"60" />

So when does OCaml decide to erase an optional argument?

The rule is: an optional argument is erased as soon as the first positional
(i.e., neither labeled nor optional) argument defined *after* the optional
argument is passed in. That explains the behavior of `prepend_pound`. But if
we had instead defined `concat` with the optional argument in the second
position.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"61" />

then application of the first argument would not cause the optional argument
to be erased.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"62" />

However, if all arguments to a function are presented at once, then erasure
of optional arguments isn't applied until all of the arguments are passed in.
This preserves our ability to pass in optional arguments anywhere on the
argument list. Thus, we can write.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"63" />

An optional argument that doesn't have any following positional arguments
can't be erased at all, which leads to a compiler warning.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"64" />

And indeed, when we provide the two positional arguments, the `sep` argument
is not erased, instead returning a function that expects the `sep` argument
to be provided.

<link rel="import" href="code/variables-and-functions/main.mlt" part=
"65" />

As you can see, OCaml's support for labeled and optional arguments is not
without its complexities. But don't let these complexities obscure the
usefulness of these features. Labels and optional arguments are very
effective tools for making your APIs both more convenient and safer, and it's
worth the effort of learning how to use them
effectively.<a data-type="indexterm" data-startref="ARGopt">&nbsp;</a><a data-type="indexterm" data-startref="FNCopt">&nbsp;</a>




