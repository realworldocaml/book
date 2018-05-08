# Variants {#variants data-type=chapter}

Variant types are one of the most useful features of OCaml and also one of
the most unusual. They let you represent data that may take on multiple
different forms, where each form is marked by an explicit tag. As we'll see,
when combined with pattern matching, variants give you a powerful way of
representing complex data and of organizing the case-analysis on that
information. [variant types/usefulness of]{.idx}[datatypes/variant
types]{.idx #DTvar}

The basic syntax of a variant type declaration is as follows: [variant
types/basic syntax of]{.idx}

<link rel="import" href="code/variants/variant.syntax" />

Each row essentially represents a case of the variant. Each case has an
associated tag and may optionally have a sequence of fields, where each field
has a specified type.

Let's consider a concrete example of how variants can be useful. Almost all
terminals support a set of eight basic colors, and we can represent those
colors using a variant. Each color is declared as a simple tag, with pipes
used to separate the different cases. Note that variant tags must be
capitalized.

<link rel="import" href="code/variants/main.mlt" part="0.5" />

The following function uses pattern matching to convert a `basic_color` to a
corresponding integer. The exhaustiveness checking on pattern matches means
that the compiler will warn us if we miss a color:

<link rel="import" href="code/variants/main.mlt" part="1" />

Using the preceding function, we can generate escape codes to change the
color of a given string displayed in a terminal:

<link rel="import" href="code/variants/main.mlt" part="2" />

On most terminals, that word "Blue" will be rendered in blue.

In this example, the cases of the variant are simple tags with no associated
data. This is substantively the same as the enumerations found in languages
like C and Java. But as we'll see, variants can do considerably more than
represent a simple enumeration. As it happens, an enumeration isn't enough to
effectively describe the full set of colors that a modern terminal can
display. Many terminals, including the venerable `xterm`, support 256
different colors, broken up into the following groups:

- The eight basic colors, in regular and bold versions

- A 6 × 6 × 6 RGB color cube

- A 24-level grayscale ramp

We'll also represent this more complicated color space as a variant, but this
time, the different tags will have arguments that describe the data available
in each case. Note that variants can have multiple arguments, which are
separated by `*`s:

<link rel="import" href="code/variants/main.mlt" part="3" />

Once again, we'll use pattern matching to convert a color to a corresponding
integer. But in this case, the pattern matching does more than separate out
the different cases; it also allows us to extract the data associated with
each tag:

<link rel="import" href="code/variants/main.mlt" part="4" />

Now, we can print text using the full set of available colors:

<link rel="import" href="code/variants/main.mlt" part="5" />

::: {.allow_break data-type=note}
#### Variants, tuples and parens

Variants with multiple arguments look an awful lot like tuples.
Consider the following example of a value of the type `color` we
defined earlier.

<link rel="import" href="code/variants/main.mlt" part="5.1" />

It really looks like we've created a 3-tuple and wrapped it with the
`RGB` constructor. But that's not what's really going on, as you can
see if create a tuple first and then place it inside the `RGB`
constructor.

<link rel="import" href="code/variants/main.mlt" part="5.2" />

We can also create variants that explicitly contain tuples, like this
one.

<link rel="import" href="code/variants/main.mlt" part="5.3" />

The syntactic difference is unfortunately quite subtle, coming down to
the extra set of parens around the arguments. But having defined it
this way, we can now take the tuple in and out freely.

<link rel="import" href="code/variants/main.mlt" part="5.4" />

If, on the other hand, we define a variant without the parens, then we
get the same behavior we got with the `RGB` constructor.

<link rel="import" href="code/variants/main.mlt" part="5.5" />

Note that, while we can't just grab the tuple as a whole from this
type, we can achieve more or less the same ends by explicitly
deconstructing and reconstructing the data we need.

<link rel="import" href="code/variants/main.mlt" part="5.6" />

The differences between a multi-argument variant and a variant
containing a tuple are mostly about performance. A multi-argument
variant is a single allocated block in memory, while a variant
containing a tuple requires an extra heap-allocated block for the
tuple. You can learn more about OCaml's memory representation in
[Memory Representation of
Values](runtime-memory-layout.html){data-type=xref}.


:::

## Catch-All Cases and Refactoring {#catch-all-cases-and-refactoring data-type=sect1}

OCaml's type system can act as a refactoring tool, warning you of places
where your code needs to be updated to match an interface change. This is
particularly valuable in the context of variants. [errors/catch-all cases and
refactoring]{.idx}[pattern matching/catch-all cases]{.idx}[functional
updates]{.idx}[refactoring]{.idx}[variant types/catch-all cases and
refactoring]{.idx}

Consider what would happen if we were to change the definition of `color` to
the following:

<link rel="import" href="code/variants/catch_all.mlt" part="1" />

We've essentially broken out the `Basic` case into two cases, `Basic` and
`Bold`, and `Basic` has changed from having two arguments to one.
`color_to_int` as we wrote it still expects the old structure of the variant,
and if we try to compile that same code again, the compiler will notice the
discrepancy:

<link rel="import" href="code/variants/catch_all.mlt" part="2" />

Here, the compiler is complaining that the `Basic` tag is used with the wrong
number of arguments. If we fix that, however, the compiler will flag a second
problem, which is that we haven't handled the new `Bold` tag:

<link rel="import" href="code/variants/catch_all.mlt" part="3" />

Fixing this now leads us to the correct implementation:

<link rel="import" href="code/variants/catch_all.mlt" part="4" />

As we've seen, the type errors identified the things that needed to be fixed
to complete the refactoring of the code. This is fantastically useful, but
for it to work well and reliably, you need to write your code in a way that
maximizes the compiler's chances of helping you find the bugs. To this end, a
useful rule of thumb is to avoid catch-all cases in pattern matches.

Here's an example that illustrates how catch-all cases interact with
exhaustion checks. Imagine we wanted a version of `color_to_int` that works
on older terminals by rendering the first 16 colors (the eight `basic_color`s
in regular and bold) in the normal way, but renders everything else as white.
We might have written the function as follows: [exhaustion checks]{.idx}

<link rel="import" href="code/variants/catch_all.mlt" part="5" />

If we then applied the same fix we did above, we would have ended up with
this.

<link rel="import" href="code/variants/catch_all.mlt" part="6" />

Because of the catch-all case, we'll no longer be warned about missing the
`Bold` case. This highlights the value of avoiding catch-all cases, since
they effectively suppress exhaustiveness checking.

## Combining Records and Variants {#combining-records-and-variants data-type=sect1}

The term *algebraic data types* is often used to describe a collection of
types that includes variants, records, and tuples. Algebraic data types act
as a peculiarly useful and powerful language for describing data. At the
heart of their utility is the fact that they combine two different kinds of
types: *product types*, like tuples and records, which combine multiple
different types together and are mathematically similar to Cartesian
products; and *sum types*, like variants, which let you combine multiple
different possibilities into one type, and are mathematically similar to
disjoint unions.[records/and variant types]{.idx #RECvartyp}[sum
types]{.idx}[product types]{.idx}[datatypes/algebraic types]{.idx}[algebraic
data types]{.idx}[variant types/and records]{.idx #VARTYPrec}

Algebraic data types gain much of their power from the ability to construct
layered combinations of sums and products. Let's see what we can achieve with
this by revisiting the logging server types that were described in
[Records](records.html#records){data-type=xref}. We'll start by reminding
ourselves of the definition of `Log_entry.t`:

<link rel="import" href="code/variants/logger.mlt" part="1" />

This record type combines multiple pieces of data into one value. In
particular, a single `Log_entry.t` has a `session_id`*and* a `time`*and* an
`important` flag *and* a `message`. More generally, you can think of record
types as conjunctions. Variants, on the other hand, are disjunctions, letting
you represent multiple possibilities, as in the following example:

<link rel="import" href="code/variants/logger.mlt" part="2" />

A `client_message` is a `Logon`*or* a `Heartbeat`*or* a `Log_entry`. If we
want to write code that processes messages generically, rather than code
specialized to a fixed message type, we need something like `client_message`
to act as one overarching type for the different possible messages. We can
then match on the `client_message` to determine the type of the particular
message being dealt with.

You can increase the precision of your types by using variants to represent
differences between types, and records to represent shared structure.
Consider the following function that takes a list of `client_message`s and
returns all messages generated by a given user. The code in question is
implemented by folding over the list of messages, where the accumulator is a
pair of:

- The set of session identifiers for the user that have been seen thus far

- The set of messages so far that are associated with the user

Here's the concrete code:

<link rel="import" href="code/variants/logger.mlt" part="3" />

Note that we take advantage of the fact that the type of the record `m` is
known in the above code, so we don't have to qualify the record fields by the
module they come from. *e.g.*, we write `m.user` instead of `m.Logon.user`.

One annoyance of the above code is that the logic for determining the session
ID is somewhat repetitive, contemplating each of the possible message types
(including the `Logon` case, which isn't actually possible at that point in
the code) and extracting the session ID in each case. This per-message-type
handling seems unnecessary, since the session ID works the same way for all
of message types.

We can improve the code by refactoring our types to explicitly reflect the
information that's shared between the different messages. The first step is
to cut down the definitions of each per-message record to contain just the
information unique to that record:

<link rel="import" href="code/variants/logger.mlt" part="4" />

We can then define a variant type that combines these types:

<link rel="import" href="code/variants/logger.mlt" part="5" />

Separately, we need a record that contains the fields that are common across
all messages:

<link rel="import" href="code/variants/logger.mlt" part="6" />

A full message can then be represented as a pair of a `Common.t` and a
`details`. Using this, we can rewrite our preceding example as follows. Note
that we add extra type annotations so that OCaml recognizes the record fields
correctly. Otherwise, we'd need to qualify them explicitly.

<link rel="import" href="code/variants/logger.mlt" part="7" />

As you can see, the code for extracting the session ID has been replaced with
the simple expression `common.session_id`.

In addition, this design allows us to grab the specific message and dispatch
code to handle just that message type. In particular, while we use the type
`Common.t * details` to represent an arbitrary message, we can use
`Common.t * Logon.t` to represent a logon message. Thus, if we had functions
for handling individual message types, we could write a dispatch function as
follows:

<link rel="import" href="code/variants/logger.mlt" part="8" />

And it's explicit at the type level that `handle_log_entry` sees only
`Log_entry` messages, `handle_logon` sees only `Logon` messages, etc.
<a data-type="indexterm" data-startref="RECvartyp">&nbsp;</a><a data-type="indexterm" data-startref="VARTYPrec">&nbsp;</a>

### Embedded records {#embedded-records data-type=sect2}

If we don't need to be able to pass the record types separately from the
variant, then OCaml allows us to embed the records directly into the variant.

<link rel="import" href="code/variants/logger.mlt" part="9" />

Even though the type is different, we can write `messages_for_user` in
essentially the same way we did before.

<link rel="import" href="code/variants/logger.mlt" part="10" />

Variants with inline records are both more concise and more efficient than
having variants containing references to free-standing record types, because
they don't require a separate allocated object for the contents of the
variant.

The main downside is the obvious one, which is that an inline record can't be
treated as its own free-standing object. And, as you can see below, OCaml
will reject code that tries to do so.

<link rel="import" href="code/variants/logger.mlt" part="11" />


## Variants and Recursive Data Structures {#variants-and-recursive-data-structures data-type=sect1}

Another common application of variants is to represent tree-like recursive
data structures. We'll show how this can be done by walking through the
design of a simple Boolean expression language. Such a language can be useful
anywhere you need to specify filters, which are used in everything from
packet analyzers to mail clients. [recursive data structures]{.idx}[data
structures/recursive]{.idx}[variant types/and recursive data
structures]{.idx}

An expression in this language will be defined by the variant `expr`, with
one tag for each kind of expression we want to support:

<link rel="import" href="code/variants/blang.mlt" part="0.5" />

Note that the definition of the type `expr` is recursive, meaning that a
`expr` may contain other `expr`s. Also, `expr` is parameterized by a
polymorphic type `'a` which is used for specifying the type of the value that
goes under the `Base` tag.

The purpose of each tag is pretty straightforward. `And`, `Or`, and `Not` are
the basic operators for building up Boolean expressions, and `Const` lets you
enter the constants `true` and `false`.

The `Base` tag is what allows you to tie the `expr` to your application, by
letting you specify an element of some base predicate type, whose truth or
falsehood is determined by your application. If you were writing a filter
language for an email processor, your base predicates might specify the tests
you would run against an email, as in the following example:

<link rel="import" href="code/variants/blang.mlt" part="1" />

Using the preceding code, we can construct a simple expression with
`mail_predicate` as its base predicate:

<link rel="import" href="code/variants/blang.mlt" part="2" />

Being able to construct such expressions isn't enough; we also need to be
able to evaluate them. Here's a function for doing just that:

<link rel="import" href="code/variants/blang.mlt" part="3" />

The structure of the code is pretty straightforward—we're just pattern
matching over the structure of the data, doing the appropriate calculation
based on which tag we see. To use this evaluator on a concrete example, we
just need to write the `base_eval` function, which is capable of evaluating a
base predicate.

Another useful operation on expressions is simplification. The following is a
set of simplifying construction functions that mirror the tags of an
`expr`:

<link rel="import" href="code/variants/blang.mlt" part="4" />

We can now write a simplification routine that is based on the preceding
functions.

<link rel="import" href="code/variants/blang.mlt" part="5" />

We can apply this to a Boolean expression and see how good a job it does at
simplifying it:

<link rel="import" href="code/variants/blang.mlt" part="6" />

Here, it correctly converted the `Or` branch to `Const true` and then
eliminated the `And` entirely, since the `And` then had only one nontrivial
component.

There are some simplifications it misses, however. In particular, see what
happens if we add a double negation in:

<link rel="import" href="code/variants/blang.mlt" part="7" />

It fails to remove the double negation, and it's easy to see why. The
`not_` function has a catch-all case, so it ignores everything but the one
case it explicitly considers, that of the negation of a constant. Catch-all
cases are generally a bad idea, and if we make the code more explicit, we see
that the missing of the double negation is more obvious:

<link rel="import" href="code/variants/blang.mlt" part="8" />

We can of course fix this by simply adding an explicit case for double
negation:

<link rel="import" href="code/variants/blang.mlt" part="9" />

The example of a Boolean expression language is more than a toy. There's a
module very much in this spirit in `Base` called `Blang` (short for "Boolean
language"), and it gets a lot of practical use in a variety of applications.
The simplification algorithm in particular is useful when you want to use it
to specialize the evaluation of expressions for which the evaluation of some
of the base predicates is already known.

More generally, using variants to build recursive data structures is a common
technique, and shows up everywhere from designing little languages to
building complex data structures.

## Polymorphic Variants {#polymorphic-variants data-type=sect1}

In addition to the ordinary variants we've seen so far, OCaml also supports
so-called *polymorphic variants*. As we'll see, polymorphic variants are more
flexible and syntactically more lightweight than ordinary variants, but that
extra power comes at a cost. [polymorphic variant types/basic syntax
of]{.idx}[variant types/polymorphic]{.idx #VARTYPpoly}

Syntactically, polymorphic variants are distinguished from ordinary variants
by the leading backtick. And unlike ordinary variants, polymorphic variants
can be used without an explicit type declaration:

<link rel="import" href="code/variants/main.mlt" part="6" />

As you can see, polymorphic variant types are inferred automatically, and
when we combine variants with different tags, the compiler infers a new type
that knows about all of those tags. Note that in the preceding example, the
tag name (e.g., `` `Int``) matches the type name (`int`). This is a common
convention in OCaml. [polymorphic variant types/automatic inference of]{.idx}

The type system will complain if it sees incompatible uses of the same tag:

<link rel="import" href="code/variants/main.mlt" part="7" />

The `>` at the beginning of the variant types above is critical because it
marks the types as being open to combination with other variant types. We can
read the type `` [> `Int of string | `Float of float]`` as describing a
variant whose tags include `` `Int of string`` and `` `Float of float``, but
may include more tags as well. In other words, you can roughly translate
`>` to mean: "these tags or more."

OCaml will in some cases infer a variant type with `<`, to indicate "these
tags or less," as in the following example:

<link rel="import" href="code/variants/main.mlt" part="8" />

The `<` is there because `is_positive` has no way of dealing with values that
have tags other than `` `Float of float`` or `` `Int of int``.

We can think of these `<` and `>` markers as indications of upper and lower
bounds on the tags involved. If the same set of tags are both an upper and a
lower bound, we end up with an *exact* polymorphic variant type, which has
neither marker. For example:

<link rel="import" href="code/variants/main.mlt" part="9" />

Perhaps surprisingly, we can also create polymorphic variant types that have
different upper and lower bounds. Note that `Ok` and `Error` in the following
example come from the `Result.t` type from `Base`: [polymorphic variant
types/upper/lower bounds of]{.idx}

<link rel="import" href="code/variants/main.mlt" part="10" />

Here, the inferred type states that the tags can be no more than `` `Float``,
`` `Int``, and `` `Not_a_number``, and must contain at least `` `Float`` and
`` `Int``. As you can already start to see, polymorphic variants can lead to
fairly complex inferred types.

### Example: Terminal Colors Redux {#example-terminal-colors-redux data-type=sect2}

To see how to use polymorphic variants in practice, we'll return to terminal
colors. Imagine that we have a new terminal type that adds yet more colors,
say, by adding an alpha channel so you can specify translucent colors. We
could model this extended set of colors as follows, using an ordinary
variant:[polymorphic variant types/vs. ordinary variants]{.idx}

<link rel="import" href="code/variants/main.mlt" part="11" />

We want to write a function `extended_color_to_int`, that works like
`color_to_int` for all of the old kinds of colors, with new logic only for
handling colors that include an alpha channel. One might try to write such a
function as follows.

<link rel="import" href="code/variants/main.mlt" part="12" />

The code looks reasonable enough, but it leads to a type error because
`extended_color` and `color` are in the compiler's view distinct and
unrelated types. The compiler doesn't, for example, recognize any equality
between the `Basic` tag in the two types.

What we want to do is to share tags between two different variant types, and
polymorphic variants let us do this in a natural way. First, let's rewrite
`basic_color_to_int` and `color_to_int` using polymorphic variants. The
translation here is pretty straightforward:

<link rel="import" href="code/variants/main.mlt" part="13" />

Now we can try writing `extended_color_to_int`. The key issue with this code
is that `extended_color_to_int` needs to invoke `color_to_int` with a
narrower type, i.e., one that includes fewer tags. Written properly, this
narrowing can be done via a pattern match. In particular, in the following
code, the type of the variable `color` includes only the tags `` `Basic``,
`` `RGB``, and `` `Gray``, and not `` `RGBA``:

<link rel="import" href="code/variants/main.mlt" part="14" />

The preceding code is more delicately balanced than one might imagine. In
particular, if we use a catch-all case instead of an explicit enumeration of
the cases, the type is no longer narrowed, and so compilation fails:

<link rel="import" href="code/variants/main.mlt" part="15" />

::: {.allow_break data-type=note}
#### Polymorphic Variants and Catch-all Cases

As we saw with the definition of `is_positive`, a `match` statement can lead
to the inference of an upper bound on a variant type, limiting the possible
tags to those that can be handled by the match. If we add a catch-all case to
our `match` statement, we end up with a type with a lower bound:[pattern
matching/catch-all cases]{.idx}[catch-all cases]{.idx}[polymorphic variant
types/and catch-all cases]{.idx}

<link rel="import" href="code/variants/main.mlt" part="16" />

Catch-all cases are error-prone even with ordinary variants, but they are
especially so with polymorphic variants. That's because you have no way of
bounding what tags your function might have to deal with. Such code is
particularly vulnerable to typos. For instance, if code that uses
`is_positive_permissive` passes in `Float` misspelled as `Floot`, the
erroneous code will compile without complaint:

<link rel="import" href="code/variants/main.mlt" part="17" />

With ordinary variants, such a typo would have been caught as an unknown tag.
As a general matter, one should be wary about mixing catch-all cases and
polymorphic variants.
:::


Let's consider how we might turn our code into a proper library with an
implementation in an `ml` file and an interface in a separate `mli`, as we
saw in
[Files Modules And Programs](files-modules-and-programs.html#files-modules-and-programs){data-type=xref}.
Let's start with the `mli`:

<link rel="import" href="code/variants-termcol/terminal_color.mli" />

Here, `extended_color` is defined as an explicit extension of `color`. Also,
notice that we defined all of these types as exact variants. We can implement
this library as follows:

<link rel="import" href="code/variants-termcol/terminal_color.ml" />

In the preceding code, we did something funny to the definition of
`extended_color_to_int` that underlines some of the downsides of polymorphic
variants. In particular, we added some special-case handling for the color
gray, rather than using `color_to_int`. Unfortunately, we misspelled
`Gray` as `Grey`. This is exactly the kind of error that the compiler would
catch with ordinary variants, but with polymorphic variants, this compiles
without issue. All that happened was that the compiler inferred a wider type
for `extended_color_to_int`, which happens to be compatible with the narrower
type that was listed in the `mli`.

If we add an explicit type annotation to the code itself (rather than just in
the `mli`), then the compiler has enough information to warn us:

<link rel="import" href="code/variants-termcol-annotated/terminal_color.ml" part=
"1" />

In particular, the compiler will complain that the `` `Grey`` case is unused:

<link rel="import" href="code/variants-termcol-annotated/jbuild" />

<link rel="import" href="code/variants-termcol-annotated/build.errsh" />

Once we have type definitions at our disposal, we can revisit the question of
how we write the pattern match that narrows the type. In particular, we can
explicitly use the type name as part of the pattern match, by prefixing it
with a `#`:

<link rel="import" href="code/variants-termcol-fixed/terminal_color.ml" part=
"1" />

This is useful when you want to narrow down to a type whose definition is
long, and you don't want the verbosity of writing the tags down explicitly in
the match.

### When to Use Polymorphic Variants {#when-to-use-polymorphic-variants data-type=sect2}

At first glance, polymorphic variants look like a strict improvement over
ordinary variants. You can do everything that ordinary variants can do, plus
it's more flexible and more concise. What's not to like?[polymorphic variant
types/vs. ordinary variants]{.idx}[polymorphic variant types/drawbacks
of]{.idx}

In reality, regular variants are the more pragmatic choice most of the time.
That's because the flexibility of polymorphic variants comes at a price. Here
are some of the downsides:

Complexity
: As we've seen, the typing rules for polymorphic variants are a lot more
  complicated than they are for regular variants. This means that heavy use
  of polymorphic variants can leave you scratching your head trying to figure
  out why a given piece of code did or didn't compile. It can also lead to
  absurdly long and hard to decode error messages. Indeed, concision at the
  value level is often balanced out by more verbosity at the type level.

Error-finding
: Polymorphic variants are type-safe, but the typing discipline that they
  impose is, by dint of its flexibility, less likely to catch bugs in your
  program.

Efficiency
: This isn't a huge effect, but polymorphic variants are somewhat heavier
  than regular variants, and OCaml can't generate code for matching on
  polymorphic variants that is quite as efficient as what it generated for
  regular variants.

All that said, polymorphic variants are still a useful and powerful feature,
but it's worth understanding their limitations and how to use them sensibly
and modestly.

Probably the safest and most common use case for polymorphic variants is
where ordinary variants would be sufficient but are syntactically too
heavyweight. For example, you often want to create a variant type for
encoding the inputs or outputs to a function, where it's not worth declaring
a separate type for it. Polymorphic variants are very useful here, and as
long as there are type annotations that constrain these to have explicit,
exact types, this tends to work well.

Variants are most problematic exactly where you take full advantage of their
power; in particular, when you take advantage of the ability of polymorphic
variant types to overlap in the tags they support. This ties into OCaml's
support for subtyping. As we'll discuss further when we cover objects in
[Objects](objects.html#objects){data-type=xref}, subtyping brings in a lot
of complexity, and most of the time, that's complexity you want to
avoid.<a data-type="indexterm" data-startref="VARTYPpoly">&nbsp;</a><a data-type="indexterm" data-startref="DTvar">&nbsp;</a>
