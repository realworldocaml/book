# Testing {#testing data-type=chapter}

Testing is fundamental to building reliable software, but you wouldn't
necessarily know it from watching how software engineers spend their
time.  Testing can be tedious, and in the early stages of a project,
it's often not obvious how important testing is going to become down
the line. This leads people to test less than they should, and more
critically, to design systems without taking testability into account,
which makes such omissions harder to fix down the line.

In some ways, OCaml's type-system makes this worse, by enhancing the
illusion that you can get by without testing.  After all, many trivial
bugs are caught cheaply by OCaml's type system, no testing
required. But make no mistake, with or without types, testing is
essential for developing and evolving complex software systems.

One way to improve the situation is to fix the tedium problem. With
the right tools, writing tests is lightweight and fun.  With such
tools in place, you'll find yourself writing more tests, and your
creations will be more reliable as a result.

The goal of this chapter is to teach you about some of the testing
infrastructure available in the OCaml ecosystem. But first, let's
discuss more generally what you should be optimizing for in your tests
and in your testing infrastructure.

## What makes for good tests? {#what-makes-for-good-tests data-type=sect1}

Here are some of the properties that characterize well written tests
in a good testing environment.

- **Easy to write**. The less overhead there is to adding a test, the
  more people will do it.
- **Easy to run**. Ideally, they should be run automatically, every
  time you make changes.
- **Easy to update**. Tests that are hard to adjust in the face of code
  changes can become their own form of technical debt.
- **Fast**, so they don't slow down your development process.
- **Readable**, so that someone can go back later and understand what
  the test is for.
- **Deterministic**. It's hard to take test failures seriously if
  there's a decent chance that the failure might be random.  You want
  your test failures to be believable indications of a problem, which
  requires determinism.
- **Comprehensible**. Tests whose failures are localized and easy to
  comprehend make it easier to find and fix the problem flagged by the
  failing test.

No testing framework can ensure that your tests satisfy these
properties. But the testing tools you choose can help or hinder on all
of these fronts.

As we go through the rest of this chapter, we'll try to show how the
various testing tools available for OCaml can help you on each of
these fronts.

## Inline tests {data-type=sect1}

The first step towards a good testing environment is making it easy to
set up and and run a test.  To that end, we'll show you how to write
tests with `ppx_inline_test`, which lets you add tests to any module
in your library with a specially annotated let binding.

To use inline tests, we need to enable `ppx_inline_test` as a
preprocessor, as well as tell Dune that the files in this library
contain inline tests.  To achieve the first goal, we'll add `ppx_jane`
to the set of preprocessors, which bundles together `ppx_inline_test`
with a collection of other useful preprocessors.  The second goal is
achieved by adding the `inline_tests` declaration to the library
stanza. Here's the resulting `jbuild` file.

<link rel="import" href="code/testing/simple_inline_test/jbuild" />

With this done, any module in this library can host a test. We'll
demonstrate this by creating a file called `test.ml`, containing just
a single test.

<link rel="import" href="code/testing/simple_inline_test/test.ml" />

The test passes if the expression on the right-hand side of the
equals-sign evaluates to true.  These tests are not automatically run
with the instantiation of the module, but are instead registered for
running via the test runner, which can be invoked via Dune.  While it
doesn't affect this example, it's worth noting that the test runner
will execute tests declared in different files in parallel.

<link rel="import" href="code/testing/simple_inline_test/run.sh" />

No output is generated because the test passed successfully.
But if we break the test,

<link rel="import" href="code/testing/broken_inline_test/test.ml" />

then we'll see an error when we run it.

<link rel="import" href="code/testing/broken_inline_test/run.sh" />

### More readable errors with `test_eq` {data-type=sect2}

One problem with the test output we just saw is that it doesn't show
the data associated with the failed test.  We can fix this by having
the test signal failure by throwing an exception. That exception is
then a place where we can put diagnostic information.

To do this, we need to change our test declaration to use
`let%test_unit` instead of `let%test`, so that the test allows a
unit-returning body. We're also going to use the `[%test_eq]` syntax,
which, given a type, generates code to test for equality and throw a
meaningful exception if the arguments are unequal.

Here's what our new test looks like. You'll notice that it's a little
more concise, mostly because this is a less verbose way to express the
comparison function.

<link rel="import" href="code/testing/test_eq-inline_test/test.ml" />

Here's what it looks like when we run the test.

<link rel="import" href="code/testing/test_eq-inline_test/run.sh" />

As you can see, the data that caused the comparison to fail is printed
out, along with the stacktrace that identifies the location of the
error.

### Where should tests go? {data-type=sect2}

The inline test framework lets you put tests into any `.ml` file
that's part of a library. But just because you can do something
doesn't mean you should.

Putting tests directly in the library you're building certainly has
some benefits. For one thing, it lets you put a test for a given
function directly after the defintion of that function. This approach
also lets you test aspects of your code that aren't exposed by its
external interface.

While this sounds appealing at first glance, putting tests the
approach has several downsides.

- **Test bloat**. When your tests are written as a part of your library, it
  means that every user of your library has to link in that testing
  code in their production application. Even though that code won't
  get executed in production, it still adds to the size of the
  executable.

- **Excess dependencies**. Adding testing code to your library doesn't
  just add the generated code for the specific tests you write; it can
  also require you to add dependencies on libraries and modules that
  you only really need for testing. This can further bloat your
  application, and can also require you to link libraries into your
  application that you'd rather not rely on in production.

- **Testing against unexposed APIs**. Writing tests on the inside of
  your libraries has the virtue of letting you write tests against any
  of the code you've written, not just what has been exposed in the
  API. But this is a two-edged sword.  Most of the time, it's a good
  mental discipline to express your testing in terms of the public
  API, rather than in terms of the implementation. This encourages you
  to think about and test the invariants exposed to users.

- **Readability**. Including all of your tests directly in your
  application code can make that code itself harder to read. This can
  lead to people writing too few tests in an effort to keep their
  application code uncluttered.

For all of these reasons, our recommendation is to put your tests in
test-only libraries created for that purpose.  There are some
legitimate reasons to want to put some test directly in your
production library, e.g., there's something that's really awkward to
expose in a way that makes it possible to test. But these examples are
few and far between.

::: {.allow_break data-type=note}
#### Why can't inline tests go in executables?

We've only talked about putting tests into libraries. What about
executables? After all, you want to test the logic of your
command-line tools as well. It turns out you can't do this directly,
since Dune doesn't support the `inline_tests` declaration in
executable files.

There's a good reason for this, which is that the `ppx_inline_test`
test runner needs to instantiate the modules that contain the
tests. If those modules have toplevel side-effects, that's a recipe
for disaster. You don't want your test-framework running lots of
copies of your executables in parallel without your say-so.

So, how do we test code that's part of an executable? The solution is
to break up your program in to two pieces: a directory containing a
library that contains all of the logic of your program, and is
suitable for testing (either with embedded inline tests, or from a
purpose-built testing library); and a directory for the executable
that links in the library, and is just responsible for launching the
program.

:::

## Property testing with Quickcheck {data-type=sect1}

The tests we've discussed so far have been pretty simple, amounting to
little more than writing down individual examples and checking that
their behavior is as expected.  We often want to write tests that do
more than that, and one useful form of testing that lets you step
beyond testing concrete examples is called *property testing*.

The basic idea is simple enough. A property test requires two things:
a function that takes an example input and checks that a given
property holds on that example; and a way of generating random
examples. The test then checks whether the predicate holds over many
randomly generated examples.

We can write a property test using only the tools we've learned so
far.  Here's an example.

<link rel="import" href="code/testing/manual_property_test/test.ml" />

As you can see below, this test passes.

<link rel="import" href="code/testing/manual_property_test/run.sh" />

One thing that was implicit in the example we gave above is the
probability distribution that was used for selecting
examples. Whenever you pick things at random, you're always making a
choice as to the probability with which each possible example is
selected. But not all probability distributions are equally good for
testing.  In fact, the choice we made above, which was to pick
integers uniformly and at random, is problematic, since it picks
interesting special, like zero or one, with the same probability as
everything else.

That's where Quickcheck comes in. Quickcheck is a library to help
automate the construction of testing distributions. Let's try
rewriting the example we provided above with Quickcheck.

<link rel="import" href="code/testing/quickcheck_property_test/test.ml" />

Note that we didn't explictly state how many examples should be
tested. Quickcheck has a built in default which can be overridden by
way of an optional argument.

In any case, running the test uncovers the fact that the property
we've been testing doesn't actually hold on all outputs, and
Quickcheck has found a counterexample.

<link rel="import" href="code/testing/quickcheck_property_test/run.sh" />

The example that triggers the exception is `-4611686018427387904`,
which can also be referred to as `Int.min_value`, and is the smallest
value of type `Int.t`. Note that the largest int, `Int.max_value`, is
smaller in absolute value than `Int.max_value`.

<link rel="import" href="code/testing/main.mlt" part="min-int1" />

It turns out that the standard behavior for negation is that the
negation of the minimum value of an int is equal to itself, as you can
see here:

<link rel="import" href="code/testing/main.mlt" part="min-int2" />

Quickcheck found this example for us because it's careful about the
distributions it uses, and in particular is careful about making sure
to keep track of and explore special cases like `min_value` and
`max_value`.

### Building more complex values

Tests don't subsist on simple atomic types alone, which is why you'll
often want to build distributions over more complex types. Here's a
simple example, where we want to test the behavior of
`List.rev_append`, which requires us to create lists of randomly
generated values.

<link rel="import" href="code/testing/bigger_quickcheck_test/test.ml"/>

Here, we made use of `Quickcheck.Generator.both`, which is useful for
creating a generator for pairs from two generators for the constituent
types.

<link rel="import" href="code/testing/main.mlt" part="monadic-gen1" />

Quickcheck has support for other container types, like lists and maps,
and Quickcheck's generator also form a monad, meaning that it supports
operators like `bind` and `map`, which we say in an error handling
context in
[Error
Handling](error-handling.html#bind-and-other-error-handling-idioms){data-type=xref}.

In combination with `Let_syntax`, the generator monad gives us a
convenient way to specify generators for custom types. Imagine we
wanted to generate examples of the following variant type.

<link rel="import" href="code/testing/main.mlt" part="monadic-gen2" />

Using `Let_syntax`, a generator for this would look as follows.

<link rel="import" href="code/testing/main.mlt" part="monadic-gen3" />

It may not be obvious, but throughout this function we're making
choices about the probability distribution. For example, the use of
the `union` operator means that circles, rectangles and polygons will
be equally likely. We could have used `weighted_union` to pick a
different distribution.

The full API for building generators is beyond the scope of this
chapter, but it's worth digging in to the API docs if you want more
control over the distribution of your test examples.

## Expect Tests {data-type=sect1}

While property-based tests are extremely useful, they're not always
what you want. Sometimes, instead of writing down properties, you want
to express your tests in terms of simple, concrete scenarios. *Expect
tests* are a great way of doing just that.

### Basic mechanics

An expect test involves a single source file that specifies both the
code to be executed and the expected output. Upon running an expect
test, any discrepancy between the expected output and what was
actually generated is reported, and, if the new output looks correct,
we can *promote* it to be the expected output by adjusting the source
accordingly.

Here's a simple example of a test written in this style.  Note that
the test generates output, but that output isn't captured in the
source, at least, not yet.

<link rel="import" href="code/testing/trivial_expect_test/test.ml" />

If we run the test, we'll be presented with a diff between what we
wrote, and a corrected version of the source file with an `[%expect]`
clause that contains the output generated by the test.

<link rel="import" href="code/testing/trivial_expect_test/run.sh" />

If we want to accept the corrected version of the file, we can run
`jbuilder promote`, at which point, our source file will be adjusted
to look like this.

<link rel="import" href="code/testing/trivial_expect_test_fixed/test.ml" />

Now, if we run the test again, we'll see that it passes.

<link rel="import" href="code/testing/trivial_expect_test_fixed/run.sh" />

We only have one expect block in this example, but the system supports
having multiple expect blocks, as you can see below.

<link rel="import" href="code/testing/multi_expect_test/test.ml" />

### What are expect tests good for?

It's not obvious why one would want to use expect tests in the first
place. Why should this:

<link rel="import" href="code/testing/simple_expect_test/test.ml" />

be preferable to this?

<link rel="import" href="code/testing/simple_inline_test/test.ml" />

Indeed, for examples like this, expect tests don't present a material
advantage. Simple example-based tests like the one above are a great
solution when it's easy and convenient to write out specific examples
in full. And property tests are your best bet when you have a clear
set of predicates that you want to test.

Where expect tests shine is where you want to capture some aspect of
the behavior of your system that's hard to capture in either
predicates or hand-written examples. Instead, expect tests give you a
way to visualize the behavior of your code, and then to be notified
whenever that visualization changes.

This is more useful than it might seem at first. One common use-case
of expect tests is simply to capture the behavior of a complex bit of
code that you don't necessarily have a small specification of.
