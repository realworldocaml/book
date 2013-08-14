# Error Handling

Nobody likes dealing with errors.  It's tedious, it's easy to get
wrong, and it's usually just not as fun as planning out how your
program is going to succeed.  But error handling is important, and
however much you don't like thinking about it, having your software
fail due to poor error handling code is worse.

Thankfully, OCaml has powerful tools for handling errors reliably and
with a minimum of pain.  In this chapter we'll discuss some of the
different approaches in OCaml to handling errors, and give some advice
on how to design interfaces that make error handling easier.

We'll start by describing the two basic approaches for reporting
errors in OCaml: error-aware return types and exceptions.

## Error-aware return types

The best way in OCaml to signal an error is to include that error in
your return value.  Consider the type of the `find` function in the
`List` module.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 0))
```

The option in the return type indicates that the function may not
succeed in finding a suitable element, as you can see below.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 1))
```

Having errors be explicit in the return values of your functions tells
the caller that there is an error that needs to be handled.  The
caller can then handle the error explicitly, either recovering from
the error or propagating it onward.

Consider the `compute_bounds` function defined below.  The function
takes a list and a comparison function, and returns upper and lower
bounds for the list by finding the smallest and largest element on the
list.  `List.hd` and `List.last`, which return `None` when they
encounter an empty list, are used to extract the largest and smallest
element of the list.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 2))
```

The match statement is used to handle the error cases, propagating a
None in `hd` or `last` into the return value of `compute_bounds`.

On the other hand, in `find_mismatches` below, errors encountered
during the computation do not propagate to the return value of the
function.  `find_mismatches` takes two hash tables as arguments, and
searches for keys that have different data in one table than in the
other.  As such, the failure to find a key in one table isn't a
failure of any sort.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 3))
```


The use of options to encode errors underlines the fact that it's not
clear whether a particular outcome, like not finding something on a
list, is an error or is just another valid outcome.  This depends on
the larger context of your program, and thus is not something that a
general purpose library can know in advance.  One of the advantages of
error-aware return types is that they work well in both situations.

### Encoding errors with `Result`

Options aren't always a sufficiently expressive way to report errors.
Specifically, when you encode an error as `None`, there's nowhere to
say anything about the nature of the error.

`Result.t` is meant to address this deficiency.  The type is defined
as follows.

```frag
((typ ocaml)(name error-handling/result.mli))
```

A `Result.t` is essentially an option augmented with the ability to
store other information in the error case.  Like `Some` and `None` for
options, the constructors `Ok` and `Error` are promoted to the
toplevel by `Core.Std`.  As such, we can write:

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 4))
```

without first opening the `Result` module.

### `Error` and `Or_error`

`Result.t` gives you complete freedom to choose the type of value you
use to represent errors, but it's often useful to standardize on an
error type.  Among other things, this makes it easier to write utility
functions to automate common error handling patterns.

But which type to choose?  Is it better to represent errors as
strings?  Some more structured representation like XML?  Or something
else entirely?

Core's answer to this question is the `Error.t` type, which tries to
forge a good compromise between efficiency, convenience, and control
over the presentation of errors.

It might not be obvious at first why efficiency is an issue at all.
But generating error messages is an expensive business.  An ASCII
representation of a value can be quite time-consuming to construct,
particularly if it includes expensive-to-convert numerical data.

`Error` gets around this issue through laziness.  In particular, an
`Error.t` allows you to put off generation of the error string until
and unless you need it, which means a lot of the time you never have
to construct it at all.  You can of course construct an error directly
from a string:

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 5))
```

But you can also construct an `Error.t` from a _thunk_, _i.e._, a
function that takes a single argument of type `unit`.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 6))
```

In this case, we can benefit from the laziness of `Error`, since the
thunk won't be called unless the `Error.t` is converted to a string.

The most common way to create `Error.t`s is using _s-expressions_.  An
s-expression is a balanced parenthetical expression where the leaves
of the expressions are strings.  Thus, the following is a simple
s-expression:

```frag
((typ scheme)(name error-handling/sexpr.scm))
```

S-expressions are supported by the Sexplib package that is
distributed with Core, and is the most common serialization format
used in Core.  Indeed, most types in Core come with built-in
s-expression converters.  Here's an example of creating an error using
the sexp converter for times, `Time.sexp_of_t`.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 7))
```

Note that the time isn't actually serialized into an s-expression
until the error is printed out.  We're not restricted to doing this
kind of error reporting with built-in types.  This will be discussed
in more detail in [xref](#data-serialization-with-s-expressions), but
Sexplib comes with a language extension that can autogenerate
sexp-converters for newly generated types, as shown below.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 8))
```

We can use this same idiom for generating an error.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 9))
```

`Error` also supports operations for transforming errors.  For
example, it's often useful to augment an error with some extra
information about the context of the error or to combine multiple
errors together.  `Error.tag` and `Error.of_list` fulfill these roles,
as you can see below.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 10))
```

The type `'a Or_error.t` is just a shorthand for `('a,Error.t)
Result.t`, and it is, after `option`, the most common way of returning
errors in Core.

### `bind` and other error-handling idioms

As you write more error handling code in OCaml, you'll discover that
certain patterns start to emerge.  A number of these common patterns
have been codified by functions in modules like `Option` and `Result`.
One particularly useful pattern is built around the function `bind`,
which is both an ordinary function and an infix operator `>>=`.
Here's the definition of `bind` for options.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 11))
```

As you can see, `bind None f` returns `None` without calling `f`, and
`bind (Some x) f` returns `f x`.  Perhaps surprisingly, `bind` can be
used as a way of sequencing together error-producing functions so that
the first one to produce an error terminates the computation.  Here's
a rewrite of `compute_bounds` to use a nested series of `bind`s.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 12))
```

The above code is a little bit hard to swallow, however, on a
syntactic level.  We can make it easier to read, and drop some of the
parentheses, by using the infix operator form of bind, which we get
access to by locally opening `Option.Monad_infix`.  The module is
called `Monad_infix` because the bind operator is part of a
sub-interface called `Monad`, which we'll talk about more in
[xref](#concurrent-programming-with-async).

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 13))
```

This use of `bind` isn't really materially better than the one we
started with, and indeed, for small examples like this, direct
matching of options is generally better than using `bind`.  But for
large complex examples with many stages of error-handling, the bind
idiom becomes clearer and easier to manage.

There are other useful idioms encoded in the functions in `Option`.
One example is `Option.both`, which takes two optional values and
produces a new optional pair that is `None` if either of its arguments
are `None`.  Using `Option.both`, we can make `compute_bounds` even
shorter.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 14))
```

These error-handling functions are valuable because they let you
express your error handling both explicitly and concisely.  We've only
discussed these functions in the context of the `Option` module, but
similar functionality is available in both `Result` and `Or_error`.

## Exceptions

Exceptions in OCaml are not that different from exceptions in many
other languages, like Java, C# and Python.  Exceptions are a way to
terminate a computation and report an error, while providing a
mechanism to catch and handle (and possibly recover from) exceptions
that are triggered by sub-computations.

You can trigger an exception by, for example, dividing an integer by
zero:

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 15))
```

And an exception can terminate a computation even if it happens nested
somewhere deep within it.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 16))
```

If we put a `printf` in the middle of the computation, we can see that
`List.map` is interrupted part way through it's execution, never
getting to the end of the list.


```frag
((typ ocamltop)(name error-handling/main.topscript)(part 17))
```

In addition to built-in exceptions like `Divide_by_zero`, OCaml lets
you define your own.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 18))
```

Exceptions are ordinary values, and can be manipulated just like other
OCaml values, as you can see below.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 19))
```

All exceptions are of type `exn`, and that type is a similar to a
variant type of the kind we encountered in [xref](#variants).  The
biggest difference is that it is an open type, meaning that new tags
can be added at any time, by any part of the program.  As such, you
can never have a match on an exception that is guaranteed to
exhaustively list all values.

Here's an example of a function for looking up a key in an
_association list_, _i.e._ a list of key/value pairs which uses this
newly-defined exception:

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 20))
```

Note that we named the function `find_exn` to warn the user that the
function routinely throws exceptions, a convention that is used
heavily in Core.

In the above example, `raise` throws the exception, thus terminating
the computation.  The type of raise is a bit surprising when you first
see it:

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 21))
```

The return type of `'a` suggests that `raise` could return a value of
any type.  That seems impossible, and it is.  Really, `raise` has this
type because it never returns at all. This behavior isn't restricted
to functions like `raise` that terminate by throwing exceptions.
Here's another example of a function that doesn't return a value.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 22))
```

`forever` doesn't return a value for a different reason: it is an
infinite loop.

This all matters because it means that the return type of `raise` can
be whatever it needs to be to fit in to the context it is called in.
Thus, the type system will let us throw an exception anywhere in a
program.

<note>
<title>Declaring exceptions using `with sexp`</title>

OCaml can't always generate a useful textual representation of an
exception.  For example:

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 23))
```

But if we declare the exception using `with sexp` (and the constituent
types have sexp converters), we'll get something with more
information.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 24))
```

The period in front of `Wrong_date` is there because the
representation generated by `with sexp` includes the full module path
of the module where the exception in question is defined.  In this
case, since we've declared the exception at the toplevel, that module
path is trivial.

This is all part of the support for s-expressions provided by the
Sexplib library and syntax-extension, which is described in more
detail in [xref](#data-serialization-with-s-expressions).

</note>

### Helper functions for throwing exceptions

OCaml and Core provide a number of helper functions to simplify the
task of throwing exceptions.  The simplest one is `failwith`, which
could be defined as follows:

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 25))
```

There are several other useful functions for raising exceptions, which
can be found in the API documentation for the `Common` and `Exn`
modules in Core.

Another important way of throwing an exception is the `assert`
directive.  `assert` is used for situations where a violation of the
condition in question indicates a bug.  Consider the following piece
of code for zipping together two lists.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 26))
```

Here we use `assert false`, which means that the assert is guaranteed
to trigger.  In general, one can put an arbitrary condition in the
assertion.

In this case, the assert can never be triggered because we have a
check that makes sure that the lists are of the same length before we
call `loop`.  If we change the code so that we drop this test, then we
can trigger the assert.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 27))
```

This shows what's special about `assert`, which is that it captures
the line number and character offset of the source location from which
the assertion was made.

### Exception handlers

So far, we've only seen exceptions fully terminate the execution of a
computation.  But often, we want a program to be able to respond to
and recover from an exception.  This is achieved through the use of
_exception handlers_.

In OCaml, an exception handler is declared using a `try`/`with`
statement.  Here's the basic syntax.

```frag
((typ ocamlsyntax)(name error-handling/try_with.syntax))
```

A `try/with` clause first evaluates its body, `<expr>`.  If no
exception is thrown, then the result of evaluating the body is what
the entire `try/with` clause evaluates to.

But if the evaluation of the body throws an exception, then the
exception will be fed to the pattern match statements following the
`with`.  If the exception matches a pattern, then we consider the
exception caught, and the `try/with` clause evaluates to the
expression on the right-hand side of the matching pattern.

Otherwise, the original exception continues up the stack of function
calls, to be handled by the next outer exception handler.  If the
exception is never caught, it terminates the program.

### Cleaning up in the presence of exceptions

One headache with exceptions is that they can terminate your execution
at unexpected places, leaving your program in an awkward state.
Consider the following function for loading a file full of reminders,
formatted as s-expressions.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 28))
```

The problem with this code is that the function that loads the
s-expression and parses it into a list of `Time.t`/`string` pairs
might throw an exception if the file in question is malformed.
Unfortunately, that means that the `In_channel.t` that was opened will
never be closed, leading to a file-descriptor leak.

We can fix this using Core's `protect` function.  The purpose of
`protect` is to ensure that the `finally` thunk will be called when
`f` exits, whether it exits normally or with an exception.  This is
similar to the `try/finally` construct available in many programming
languages, but it is implemented in a library, rather than being a
built-in primitive.  Here's how it could be used to fix `load_reminders`.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 29))
```

This is a common enough problem that `In_channel` has a function
called `with_file` that automates this pattern.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 30))
```

`In_channel.with_file` is actually built on top of `protect` so that
it can clean up after itself in the presence of exceptions.


### Catching specific exceptions

OCaml's exception-handling system allows you to tune your
error-recovery logic to the particular error that was thrown.  For
example, `List.find_exn` throws `Not_found` when the element in
question can't be found.  Let's look at an example of how you could
take advantage of this.  In particular, consider the following function

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 31))
```

As you can see from the type, `lookup_weight` takes an association
list, a key for looking up a corresponding value in that list, and a
function for computing a floating-point weight from the looked-up
value.  If no value is found, then a weight of `0.` should be
returned.

The use of exceptions in this code, however, presents some problems.
In particular, what happens if `compute_weight` throws an exception?
Ideally, `lookup_weight` should propagate that exception on, but if
the exception happens to be `Not_found`, then that's not what will
happen:

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 32))
```

This kind of problem is hard to detect in advance, because the type
system doesn't tell you what exceptions a given function might throw.
For this reason, it's generally better to avoid relying on the
identity of the exception to determine the nature of a failure.  A
better approach is to narrow the scope of the exception handler, so
that when it fires it's very clear what part of the code failed.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 33))
```

At which point, it makes sense to simply use the non-exception
throwing function, `List.Assoc.find`, instead.

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 34))
```

### Backtraces

A big part of the value of exceptions is that they provide useful
debugging information in the form of a stack backtrace.  Consider the
following simple program.

```frag
((typ ocaml)(name error-handling/blow_up.ml))
```

If we build and run this program, we'll get a stack backtrace that
will give you some information about where the error occurred, and the
stack of function calls that were in place at the time of the error.

```frag
((typ console)(name error-handling/build_blow_up.out))
```

You can also capture a backtrace within your program by calling
`Exn.backtrace`, which returns the backtrace of the most recently
thrown exception.  This is useful for reporting detailed information
on errors that did not cause your program to fail.

This works well if you have backtraces enabled, but that isn't always
the case.  In fact, by default, OCaml has backtraces turned off, and
even if you have them turned on at runtime, you can't get backtraces
unless you have compiled with debugging symbols.  Core reverses the
default, so if you're linking in Core, you will have backtraces
enabled at runtime.

Even using Core and compiling with debugging symbols, you can turn
backtraces off by setting the `OCAMLRUNPARAM` environment variable to
be empty.

```frag
((typ console)(name error-handling/build_blow_up_notrace.out))
```

The resulting error message is considerably less informative.  You can
also turn backtraces off in your code by calling
`Backtrace.Exn.set_recording false`.

There is a legitimate reasons to run without backtraces: speed.
OCaml's exceptions are fairly fast, but they're even faster still if
you disable backtraces.  Here's a simple benchmark that shows the
effect, using the `core_bench` package.

```frag
((typ ocaml)(name error-handling/exn_cost.ml))
```

We're testing three cases here: a simple computation with no
exceptions; the same computation with an exception handler but no
thrown exceptions; and finally the same computation where we use the
exception to do the control flow back to the caller.

If we run this with stacktraces on, the benchmark results look like
this.

```frag
((typ console)(name error-handling/run_exn_cost.out))
```

Here, we see that we lose something like 20 cycles to adding an
exception handler, and 30 more to actually throwing and catching an
exception.  If we turn backtraces off, then the results look like
this.

```frag
((typ console)(name error-handling/run_exn_cost_notrace.out))
```

Here, the handler costs about the same, at 20 cycles, but the
exception itself costs only 20, as opposed to 30 additional cycles.
All told, this should only matter if you're using exceptions routinely
as part of your flow control, which is in most cases a stylistic
mistake anyway.

### From exceptions to error-aware types and back again

Both exceptions and error-aware types are necessary parts of
programming in OCaml.  As such, you often need to move between these
two worlds.  Happily, Core comes with some useful helper functions to
help you do just that.  For example, given a piece of code that can
throw an exception, you can capture that exception into an option as
follows:

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 35))
```

And `Result` and `Or_error` have similar `try_with` functions.  So, we
could write:

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 36))
```

And then we can re-raise that exception:

```frag
((typ ocamltop)(name error-handling/main.topscript)(part 37))
```

## Choosing an error handling strategy

Given that OCaml supports both exceptions and error-aware return
types, how do you choose between them?  The key is to think about the
tradeoff between concision and explicitness.

Exceptions are more concise because they allow you to defer the job of
error handling to some larger scope, and because they don't clutter up
your types.  But this same concision comes at a cost: exceptions are
all too easy to ignore.  Error-aware return types, on the other hand,
are fully manifest in your type definitions, making the errors that
your code might generate explicit and impossible to ignore.

The right tradeoff depends on your application.  If you're writing a
rough and ready program where getting it done quickly is key, and
failure is not that expensive, then using exceptions extensively may
be the way to go.  If, on the other hand, you're writing production
software whose failure is costly, then you should probably lean in the
direction of using error-aware return types.

To be clear, it doesn't make sense to avoid exceptions entirely.  The
old maxim of "use exceptions for exceptional conditions" applies.  If
an error occurs sufficiently rarely, then throwing an exception may
well be the right behavior.  

Also, for errors that are omnipresent, error-aware return types may
also be overkill.  A good example is out-of-memory errors, which can
occur anywhere, and so you'd need to use error-aware return types
everywhere to capture those.  And having every operation marked as one
that might fail is no more explicit than having none of them marked.

In short, for errors that are a foreseeable and ordinary part of the
execution of your production code and that are not omnipresent, error
aware return types are typically the right solution.

