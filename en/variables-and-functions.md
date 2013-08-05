# Variables and Functions

Variables and functions are fundamental ideas that show up in virtually
all programming languages.  But OCaml has a different take on these
basic concepts, so we'll spend some time digging into the details so you
can understand OCaml's variables and functions and see how they differ
from what you've encountered elsewhere.

## Variables

At its simplest, a variable is an identifier whose meaning is bound to
a particular value.  In OCaml these bindings are often introduced
using the `let` keyword.  We can type a so-called _top-level_ `let`
binding into `utop` with the following syntax to bind a new variable.
Note that variable names must start with a lowercase letter or an
underscore.

```frag
((typ ocamlsyntax)(name variables-and-functions/let.syntax))
```

As we'll see when we get to the module system in
[xref](#files-modules-and-programs), this same syntax is used for let
bindings at the top-level of a module.

Every variable binding has a _scope_, which is the portion of the code
that can refer to that binding.  When using `utop`, the scope of a
top-level let binding is everything that follows it in the session.
When it shows up in a module, the scope is the remainder of that
module.

Here's a simple example.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 0)) 
```

`let` can also be used to create a variable binding whose scope is
limited to a particular expression, using the following syntax.

```frag
((typ ocamlsyntax)(name variables-and-functions/let_in.syntax))
```

This first evaluates _`<expr1>`_ and then evaluates _`<expr2>`_ with
_`<variable>`_ bound to whatever value was produced by the evaluation
of _`<expr1>`_.  Here's how it looks in practice.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 1)) 
```

Note that the scope of `language_list` is just the expression
`String.concat ~sep:"-" language_list`, and is not available at the
top level, as we can see if we try to access it now.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 2)) 
```

A let binding in an inner scope can _shadow_, or hide, the definition
from an outer scope.  So, for example, we could have written the
`dashed_languages` example as follows:

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 3)) 
```

This time, in the inner scope we called the list of strings
`languages` instead of `language_list`, thus hiding the original
definition of `languages`.  But once the definition of
`dashed_languages` is complete, the inner scope has closed and the
original definition of languages reappears.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 4)) 
```

One common idiom is to use a series of nested `let`/`in` expressions
to build up the components of a larger computation.  Thus, we might
write:

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 5)) 
```

It's important not to confuse a sequence of let bindings with the
modification of a mutable variable.  For example, consider how
`area_of_ring` would work if we had instead written this purposefully
confusing bit of code.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 6)) 
```

Here, we redefined `pi` to be zero after the definition of
`area_of_circle`.  You might think that this would mean that the
result of the computation would now be zero, but in fact, the behavior
of the function is unchanged.  That's because the original definition
of `pi` wasn't changed, it was just shadowed, which means that any
subsequent reference to `pi` would see the new definition of `pi` as
zero, but earlier references would be unchanged.  But there is no
later use of `pi`, so the binding of `0.` to `pi` made no difference.
This explains the warning produced by the toplevel telling us that
there is an unused definition of `pi`.

In OCaml, let bindings are immutable.  As we'll see in
[xref](#imperative-programming-1), there are mutable values in OCaml,
but no mutable variables.

<note> <title> Why don't variables vary?  </title>

One source of confusion for people new to OCaml is the fact that
variables are immutable.  This seems pretty surprising even on
linguistic terms.  Isn't the whole point of a variable that it can
vary?

The answer to this is that variables in OCaml (and generally in
functional languages) are really more like variables in an equation
than a variable in an imperative language.  If you think about the
mathematical equation `x(y + z) = xy + xz`, there's no notion of
mutating the variables `x`, `y` and `z`.  They vary in the sense that
you can instantiate this equation with different numbers for those
variables, and it still holds.

The same is true in a functional language.  A function can be applied
to different inputs, and thus its variables will take on different
values, even without mutation.

</note>


### Pattern matching and `let` ###

Another useful feature of let bindings is that they support the use of
_patterns_ on the left-hand side.  Consider the following code, which
uses `List.unzip`, a function for converting a list of pairs into a
pair of lists.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 7)) 
```

Here, `(ints,strings)` is a pattern, and the `let` binding assigns
values to both of the identifiers that show up in that pattern.  A
pattern is essentially a description of the shape of a data-structure,
where some components are identifiers to be bound.  As we saw in
[xref](#tuples-lists-options-and-pattern-matching), OCaml has patterns
for a variety of different data-types.

Using a pattern in a let-binding makes the most sense for a pattern
that is _irrefutable_, _i.e._, where any value of the type in question
is guaranteed to match the pattern.  Tuple and record patterns are
irrefutable, but list patterns are not.  Consider the following code
that implements a function for up-casing the first element of a
comma-separated list.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 8)) 
```

This case can't really come up in practice, because `String.split`
always returns a list with at least one element.  But the compiler
doesn't know this, and so it emits the warning.  It's generally better
to use a match statement to handle such cases explicitly:

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 9)) 
```

Note that this is our first use of `assert`, which is useful for
marking cases that should be impossible.  Asserts are discussed in
more detail in [xref](#error-handling).

## Functions ##

Given that OCaml is a functional language, it's no surprise that
functions are important and pervasive.  Indeed, functions have come up
in almost every example we've done so far.  This section will go into
more depth, explaining the details of how OCaml's functions work.  As
you'll see, functions in OCaml differ in a variety of ways from what
you'll find in most mainstream languages.

### Anonymous Functions ###

We'll start by looking at the most basic style of function declaration
in OCaml: the _anonymous function_.  An anonymous function is a
function value that is declared without being named.  They can be
declared using the `fun` keyword, as shown here.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 10)) 
```

Anonymous functions aren't named, but they can be used for many
different purposes nonetheless.  You can, for example, apply an
anonymous function to an argument.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 11)) 
```

Or pass it to another function.  Passing functions to iteration
functions like `List.map` is probably the most common use-case for
anonymous functions.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 12)) 
```

You can even stuff them into a data structure.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 13)) 
```

It's worth stopping for a moment to puzzle this example out, since
this kind of higher-order use of functions can be a bit obscure at
first.  The first thing to understand is the function `(fun g -> g
5)`, which takes a function as its argument and applies that function
to the number `5`.  The invocation of `List.map` applies `(fun g -> g
5)` to the elements of the `increments` list (which are themselves
functions) and returns the list containing the results of these
function applications.

The key thing to understand is that functions are ordinary values in
OCaml, and you can do everything with them that you'd do with an
ordinary value, including passing them to and returning them from
other functions and storing them in data structures.  We even name
functions in the same way that we name other values, by using a let
binding.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 14)) 
```

Defining named functions is so common that there is some syntactic
sugar for it.  Thus, the following definition of `plusone` is
equivalent to the definition above.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 15)) 
```

This is the most common and convenient way to declare a function, but
syntactic niceties aside, the two styles of function definition are
entirely equivalent.

<note>
<title>`let` and `fun`</title>

Functions and let bindings have a lot to do with each other.  In some
sense, you can think of the parameter of a function as a variable
being bound to the value passed by the caller.  Indeed, the following
two expressions are nearly equivalent:

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 16)) 
```

This connection is important, and will come up more when programming
in a monadic style, as we'll see in
[xref](#concurrent-programming-with-async).

</note>

### Multi-argument functions ###

OCaml of course also supports multi-argument functions, for example:

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 17)) 
```

You may find the type signature of `abs_diff` with all of its arrows a
little hard to parse.  To understand what's going on, let's rewrite
`abs_diff` in an equivalent form, using the `fun` keyword:

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 18)) 
```

This rewrite makes it explicit that `abs_diff` is actually a function
of one argument that returns another function of one argument, which
itself returns the final result.  Because the functions are nested,
the inner expression `abs (x - y)` has access to both `x`, which was
bound by the first function application, and `y`, which was bound by
the second one.

This style of function is called a _curried_ function.  (Currying is
named after Haskell Curry, a logician who had a significant impact on
the design and theory of programming languages.)  The key to
interpreting the type signature of a curried function is the
observation that `->` is right-associative.  The type signature of
`abs_diff` can therefore be parenthesized as follows.  


```frag
((typ ocaml)(name variables-and-functions/abs_diff.mli))
```

The parentheses above don't change the meaning of the signature, but
it makes it easier to see the currying.

Currying is more than just a theoretical curiosity.  You can make use
of currying to specialize a function by feeding in some of the
arguments.  Here's an example where we create a specialized version of
`abs_diff` that measures the distance of a given number from `3`.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 19)) 
```

The practice of applying some of the arguments of a curried function
to get a new function is called _partial application_.

Note that the `fun` keyword supports its own syntax for currying, so
the following definition of `abs_diff` is equivalent to the definition
above.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 20)) 
```

You might worry that curried functions are terribly expensive, but
this is not the case.  In OCaml, there is no penalty for calling a
curried function with all of its arguments.  (Partial application,
unsurprisingly, does have a small extra cost.)

Currying is not the only way of writing a multi-argument function in
OCaml.  It's also possible to use the different parts of a tuple as
different arguments.  So, we could write:

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 21)) 
```

OCaml handles this calling convention efficiently as well.  In
particular it does not generally have to allocate a tuple just for the
purpose of sending arguments to a tuple-style function.  (You can't,
however, use partial application for this style of function.)

There are small tradeoffs between these two approaches, but most of
the time, one should stick to currying, since it's the default style
in the OCaml world.

### Recursive functions ###

A function is _recursive_ if it refers to itself in its definition.
Recursion is important in any programming language, but is
particularly important in functional languages, because it is the
fundamental building block that is used for building looping
constructs.  (As we'll see in [xref](#imperative-programming-1), OCaml
also supports imperative looping constructs like `for` and `while`,
but these are only useful when using OCaml's imperative features.)

In order to define a recursive function, you need to mark the let
binding as recursive with the `rec` keyword, as shown in this function
for finding the first sequentially-repeated element in a list.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 22)) 
```

Note that in the above, the pattern `| [] | [_]` is what's called on
_or-pattern_, which is the combination of two patterns.  In this case,
`[]`, matching the empty list, and `[_]`, matching any single element
list.  The `_` is there so we don't have to put an explicit name on
that single element.

We can also define multiple mutually recursive values by using `let
rec` combined with the `and` keyword.  Here's a (gratuitously
inefficient) example.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 23)) 
```

OCaml distinguishes between non-recursive definitions (using `let`)
and recursive definitions (using `let rec`) largely for technical
reasons: the type-inference algorithm needs to know when a set of
function definitions are mutually recursive, and for reasons that
don't apply to a pure language like Haskell, these have to be marked
explicitly by the programmer.

But this decision has some good effects.  For one thing, recursive
(and especially mutually recursive) definitions are harder to reason
about than non-recursive definitions that proceed in order, each
building on top of what has already been defined.  It's therefore
useful that, in the absence of an explicit marker, new definitions can
only build upon ones that were previously defined.

In addition, having a non-recursive form makes it easier to create a
new definition that extends and supersedes an existing one by
shadowing it.

### Prefix and Infix operators ###

So far, we've seen examples of functions used in both prefix and infix
style:

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 24)) 
```

You might not have thought of the second example as an ordinary
function, but it very much is.  Infix operators like `+` really only
differ syntactically from other functions.  In fact, if we put
parentheses around an infix operator, you can use it as an ordinary
prefix function.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 25)) 
```

In the second expression above, we've partially applied `(+)` to gain
a function that increments its single argument by `3`, and then
applied that to all the elements of a list.

A function is treated syntactically as an operator if the name of that
function is chosen from one of a specialized set of identifiers.  This
set includes identifiers that are sequences of characters from the
following set:

```frag
((typ ocamlsyntax)(name variables-and-functions/operators.syntax))
```

or is one of a handful of pre-determined strings, including `mod`, the
modulus operator, and `lsl`, for "logical shift left", a bit-shifting
operation.

We can define (or redefine) the meaning of an operator.  Here's an
example of a simple vector-addition operator on int pairs.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 26)) 
```

Note that you have to be careful when dealing with operators containing
`*`.  Consider the following example.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 27)) 
```

What's going on is that `(***)` isn't interpreted as an operator at
all; it's read as a comment!  To get this to work properly, we need to
put spaces around any operator that begins or ends with `*`.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 28)) 
```

The syntactic role of an operator is typically determined by its first
character or two, though there are a few exceptions.  This table
breaks the different operators and other syntactic forms into groups
from highest to lowest precedence, explaining how each behaves
syntactically.  We write `!`... to indicate the class of operators
beginning with `!`.

------------------------------------------
Prefix                     Usage
-----------------------    -----------------
`!`..., `?`..., `~`...     Prefix

`.`, `.(`, `.[`

function application,      Left associative
constructor, `assert`,
`lazy`

`-`, `-.`                  Prefix

`**`...,                   Right associative
`lsl`, `lsr`, `asr`

`*`..., `/`..., `%`...,    Left associative
`mod`, `land`, `lor`,
`lxor`

`+`, `-`                   Left associative

`::`                       Right associative

`@`..., `^`...             Right associative

`=`..., `<`..., `>`...,    Left associative
`|`..., `&`..., `$`...

`&`, `&&`                  Right associative

`or`, `||`                 Right associative

`,`

`<-`, `:=`                 Right associative

`if`

`;`                        Right associative

--------------------------------------------

There's one important special case: `-` and `-.`, which are the
integer and floating-point subtraction operators, can act as both
prefix operators (for negation) and infix operators (for subtraction),
So, both `-x` and `x - y` are meaningful expressions.  Another thing
to remember about negation  is that it has lower precedence than
function application, which means that if you want to pass a negative
value, you need to wrap it in parentheses, as you can see below.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 29)) 
```

Here, OCaml is interpreting the second expression as equivalent to:

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 30))
```

which obviously doesn't make sense.

Here's an example of a very useful operator from the standard library
whose behavior depends critically on the precedence rules described
above.  Here's the code.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 31)) 
```

It's not quite obvious at first what the purpose of this operator is:
it just takes a value and a function, and applies the function to the
value.  Despite that bland sounding description, it has the useful
role of a sequencing operator, similar in spirit to using pipe in the
UNIX shell.  Consider, for example, the following code for printing
out the unique elements of your `PATH`.  Note that `List.dedup` below
removes duplicates from a list by sorting the list using the provided
comparison function.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 32)) 
```

Note that we can do this without `|>`, but the result is a bit more
verbose.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 33)) 
```


An important part of what's happening here is partial application.
For example, `List.iter` normally takes two arguments: a function to
be called on each element of the list, and the list to iterate over.
We can call `List.iter` with all its arguments:

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 34)) 
```

Or, we can pass it just the function argument, leaving us with a
function for printing out a list of strings.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 35)) 
```

It is this later form that we're using in the `|>` pipeline above.

Note that `|>` only works in the intended way because it is
left-associative.  Indeed, let's see what happens if we try using a
right associative operator, like (^>).

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 36)) 
```

The above type error is a little bewildering at first glance.  What's
going on is that, because `^>` is right associative, the operator is
trying to feed the value `List.dedup ~compare:String.compare` to the
function `List.iter ~f:print_endline`.  But `List.iter
~f:print_endline` expects a list of strings as its input, not a
function.

The type error aside, this example highlights the importance of
choosing the operator you use with care, particularly with respect to
associativity.

### Declaring functions with `function` ###

Another way to define a function is using the `function` keyword.
Instead of having syntactic support for declaring multi-argument
(curried) functions, `function` has built-in pattern matching.  Here's
an example:

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 37)) 
```

This is equivalent to combining an ordinary function definition with a
`match`.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 38)) 
```

We can also combine the different styles of function declaration
together, as in the following example where we declare a two argument
(curried) function with a pattern match on the second argument.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 39)) 
```

Also, note the use of partial application to generate the function
passed to `List.map`.  In other words, `some_or_default 100` is a
function that was created by feeding just the first argument to
`some_or_default`.

### Labeled Arguments ###

Up until now, the functions we've defined have specified their arguments
positionally, _i.e._, by the order in which the arguments are passed to
the function.  OCaml also supports labeled arguments, which let you
identify a function argument by name.  Indeed, we've already encountered
functions from Core like `List.map` that use labeled arguments.  Labeled
arguments are marked by a leading tilde, and a label (followed by a
colon) are put in front of the variable to be labeled.  Here's an
example.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 40)) 
```

We can then provide a labeled argument using a similar convention.  As
you can see, the arguments can be provided in any order.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 41)) 
```

OCaml also supports _label punning_, meaning that you get to drop the
text after the `:` if the name of the label and the name of the
variable being used are the same.  We were actually already using
label punning when defining our labeled function above.  The following
shows how punning can be used when invoking a function.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 42)) 
```

Labeled arguments are useful in a few different cases:

  - When defining a function with lots of arguments.  Beyond a certain
    number, arguments are easier to remember by name than by position.

  - When the meaning of a particular argument is unclear from the type
    alone.  Consider a function for creating a hash table whose first
    argument is the initial size of the array backing the hashtable,
    and the second is a Boolean flag which indicates whether that
    array will ever shrink when elements are removed.  

    ```frag
    ((typ ocaml)(name variables-and-functions/htable_sig1.ml))
    ```

    The above signature makes it hard to divine the meaning of those
    two arguments.  but with labeled arguments, we can make the intent
    immediately clear.

    ```frag
    ((typ ocaml)(name variables-and-functions/htable_sig2.ml))
    ```

    Choosing label names well is especially important for Boolean
    values, since it's often easy to get confused about whether a
    value being true is meant ot enable or disable a given feature.

  - When defining functions that have multiple arguments that might
    get confused with each other.  This is most at issue when the
    arguments are of the same type.  For example, consider this
    signature for a function that extracts a substring.

    ```frag
    ((typ ocaml)(name variables-and-functions/substring_sig1.ml))
    ```

    Here, the two ints are the starting position and length of the
    substring to extract, respectively.  We can can make this fact
    more obvious from the signature by adding labels.

    ```frag
    ((typ ocaml)(name variables-and-functions/substring_sig2.ml))
    ```

    This improves the readability of both the signature and of client
    code that makes use of `substring`, and makes it harder to
    accidentally swap the position and the length.

  - When you want flexibility on the order in which arguments are
    passed.  Consider a function like `List.iter`, that takes two
    arguments: a function, and a list of elements to call that
    function on.  A common pattern is to partially apply `List.iter`
    by giving it just the function, as in the following example from
    earlier in the chapter.

    ```frag
    ((typ ocamltop)(name variables-and-functions/main.topscript)(part 43)) 
    ```

    This requires that we put the function argument first.  In other
    cases, you want to put the function argument second.  One common
    reason is readability.  In particular, a multi-line function
    passed as an argument to another function is easiest to read when
    it is the final argument to that function.

#### Higher-order functions and labels ####

One surprising gotcha with labeled arguments is that while order
doesn't matter when calling a function with labeled arguments, it does
matter in a higher-order context, _e.g._, when passing a function with
labeled arguments to another function.  Here's an example.


```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 44)) 
```

Here, the definition of `apply_to_tuple` sets up the expectation that
its first argument is a function with two labeled arguments, `first`
and `second`, listed in that order.  We could have defined
`apply_to_tuple` differently to change the order in which the labeled
arguments were listed.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 45)) 
```

It turns out this order matters.  In particular, if we define a
function that has a different order

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 46)) 
```

we'll find that it can't be passed in to `apply_to_tuple_2`.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 47)) 
```

But, it works smoothly with the original `apply_to_tuple`.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 48)) 
```

As a result, when passing labeled functions as arguments, you need to
take care to be consistent in your ordering of labeled arguments in
higher-order contexts.

### Optional arguments ###

An optional argument is like a labeled argument that the caller can
choose whether or not to provide.  Optional arguments are passed in
using the same syntax as labeled arguments, and, like labeled
arguments, can be provided in any order.

Here's an example of a string concatenation function with an optional
separator.  This function uses the `^` operator for simple pairwise
string concatenation.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 49)) 
```

Here, `?` is used in the definition of the function to mark `sep` as
optional.  And while the caller can pass a value of type `string` for
`sep`, internally to the function, `sep` is seen as a `string option`,
with `None` appearing when `sep` is not provided by the caller.

The above example needed a bit of boilerplate to choose a default
separator when none was provided.  This is a common enough pattern
that there's an explicit syntax for providing a default value, which
allows us to write `concat` more concisely.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 50)) 
```

Optional arguments are very useful, but they're also easy to abuse.
The key advantage of optional arguments is that they let you write
functions with multiple arguments that users can ignore most of the
time, only worrying about them when they specifically want to invoke
those options.  They also allow you to extend an API with new
functionality without changing existing code that calls that function.

The downside is that the caller may be unaware that there is a choice
to be made, and so may unknowingly (and wrongly) pick the default
behavior.  Optional arguments really only make sense when the extra
concision of omitting the argument outweighs the corresponding loss of
explicitness.

This means that rarely used functions should not have optional
arguments.  A good rule of thumb is to avoid optional arguments for
functions internal to a module, _i.e._, functions that are not
included in the module's interface, or `mli` file.  We'll learn more
about `mli`s in [xref](#files-modules-and-programs).

#### Explicit passing of an optional argument ###

Under the covers, a function with an optional argument receives `None`
when the caller doesn't provide the argument, and `Some` when it does.
But the `Some` and `None` are normally not explicitly passed in by the
caller.

But sometimes, passing in `Some` or `None` explicitly is exactly what
you want.  OCaml lets you do this by using `?` instead of `~` to mark
the argument.  Thus, the following two lines are equivalent ways of
specifying the `sep` argument to concat.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 51)) 
```

And the following two lines are equivalent ways of calling `concat`
without specifying `sep`.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 52)) 
```

One use-case for this is when you want to define a wrapper function
that mimics the optional arguments of the function it's wrapping.  For
example, imagine we wanted to create a function called
`uppercase_concat`, which is the same as `concat` except that it
converts the first string that it's passed to uppercase.  We could
write the function as follows.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 53)) 
```

In the way we've written it, we've been forced to separately make the
decision as to what the default separator is.  Thus, if we later
change `concat`'s default behavior, we'll need to remember to change
`uppercase_concat` to match it.

Instead, we can have `uppercase_concat` simply pass through the
optional argument to `concat` using the `?` syntax.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 54)) 
```

Now, if someone calls `uppercase_concat` without an argument, an
explicit `None` will be passed to `concat`, leaving `concat` to decide
what the default behavior should be.

#### Inference of labeled and optional arguments

One subtle aspect of labeled and optional arguments is how they are
inferred by the type system.  Consider the following example for
computing numerical derivatives of a function of two dimensions.  The
function takes an argument `delta` which determines the scale at which
to compute the derivative, values `x` and `y` which determine which
point to compute the derivative at, and the function `f` whose
derivative is being computed.  The function `f` itself takes two
labeled arguments `x` and `y`.  Note that you can use an apostrophe as
part of a variable name, so `x'` and `y'` are just ordinary variables.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 55)) 
```

In principle, it's not obvious how the order of the arguments to `f`
should be chosen.  Since labeled arguments can be passed in arbitrary
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
different invocations of `f` list the arguments in different orders.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 56)) 
```

As suggested by the error message, we can get OCaml to accept the fact
that `f` is used with different argument orders if we provide explicit
type information.  Thus, the following code compiles without error,
due to the type annotation on `f`.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 57)) 
```

#### Optional arguments and partial application ###

Optional arguments can be tricky to think about in the presence of
partial application.  We can of course partially apply the optional
argument itself:

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 58)) 
```

But what happens if we partially apply just the first argument?

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 59)) 
```

The optional argument `?sep` has now disappeared, or been _erased_.
Indeed, if we try to pass in that optional argument now, it will be
rejected.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 60)) 
```

So when does OCaml decide to erase an optional argument?

The rule is: an optional argument is erased as soon as the first
positional (_i.e._, neither labeled nor optional) argument defined
_after_ the optional argument is passed in.  That explains the
behavior of `prepend_pound` above.  But if we had instead defined
`concat` with the optional argument in the second position:

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 61)) 
```

then application of the first argument would not cause the optional
argument to be erased.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 62)) 
```

However, if all arguments to a function are presented at once, then
erasure of optional arguments isn't applied until all of the arguments
are passed in.  This preserves our ability to pass in optional
arguments anywhere on the argument list.  Thus, we can write:

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 63)) 
```

An optional argument that doesn't have any following positional
arguments can't be erased at all, which leads to a compiler warning.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 64)) 
```

And indeed, when we provide the two positional arguments, the `sep`
argument is not erased, instead returning a function that expects the
`sep` argument to be provided.

```frag
((typ ocamltop)(name variables-and-functions/main.topscript)(part 65)) 
```

